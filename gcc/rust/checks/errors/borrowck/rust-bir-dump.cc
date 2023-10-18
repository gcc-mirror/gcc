#include "rust-bir-dump.h"

namespace Rust {
namespace BIR {

constexpr auto indentation = "    ";

uint32_t
get_place_name (PlaceId place_id)
{
  rust_assert (place_id >= FIRST_VARIABLE_PLACE);
  return place_id - FIRST_VARIABLE_PLACE;
}

uint32_t
get_lifetime_name (Lifetime lifetime_id)
{
  rust_assert (lifetime_id.id >= FIRST_NORMAL_LIFETIME_ID);
  // Start from 1 as rustc does.
  return lifetime_id.id - FIRST_NORMAL_LIFETIME_ID + 1;
}

std::string
get_tyty_name (TyTy::BaseType *tyty)
{
  if (tyty)
    return tyty->get_name ();
  return "unknown";
}

void
Dump::go ()
{
  stream << "fn " << name << "(";
  for (PlaceId arg : func.arguments)
    {
      stream << "_" << get_place_name (arg) << ": "
	     << get_tyty_name (place_db[arg].tyty) << ", ";
    }
  stream << ") -> " << get_tyty_name (place_db[RETURN_VALUE_PLACE].tyty)
	 << " {\n";

  for (PlaceId id = FIRST_VARIABLE_PLACE; id < place_db.size (); ++id)
    {
      const Place &place = place_db[id];
      if (place.kind == Place::VARIABLE || place.kind == Place::TEMPORARY)
	{
	  if (std::find (func.arguments.begin (), func.arguments.end (), id)
	      != func.arguments.end ())
	    continue;
	  stream << indentation << "let _" << get_place_name (id) << ": "
		 << get_tyty_name (place_db[id].tyty) << ";\n";
	}
    }

  for (BasicBlockId id = 0; id < func.basic_blocks.size (); ++id)
    {
      if (func.basic_blocks[id].statements.empty ()
	  && func.basic_blocks[id].successors.empty ())
	continue;

      BasicBlock &bb = func.basic_blocks[id];
      stream << "\n";
      stream << indentation << "bb" << id << ": {\n";
      for (auto &stmt : bb.statements)
	{
	  stream << indentation << indentation;
	  visit (stmt);
	  stream << ";\n";
	}
      stream << indentation << "} -> [";
      for (auto succ : bb.successors)
	stream << "bb" << succ << ", ";
      stream << "]\n";
    }

  stream << "}\n\n";
}
void
Dump::visit (Node &node)
{
  node_place = node.get_place ();
  switch (node.get_kind ())
    {
      case Node::Kind::ASSIGNMENT: {
	stream << "_" << get_place_name (node.get_place ()) << " = ";
	node.get_expr ().accept_vis (*this);
	break;
      }
    case Node::Kind::SWITCH:
      stream << "switch ";
      visit_move_place (node.get_place ());
      break;
    case Node::Kind::RETURN:
      stream << "return";
      break;
    case Node::Kind::GOTO:
      stream << "goto";
      break;
    case Node::Kind::STORAGE_DEAD:
      stream << "StorageDead(";
      visit_move_place (node.get_place ());
      stream << ")";
      break;
    case Node::Kind::STORAGE_LIVE:
      stream << "StorageLive(";
      visit_move_place (node.get_place ());
      stream << ")";
      break;
    }
  node_place = INVALID_PLACE;
}

void
Dump::visit_place (PlaceId place_id)
{
  const Place &place = place_db[place_id];
  switch (place.kind)
    {
    case Place::TEMPORARY:
    case Place::VARIABLE:
      stream << "_" << get_place_name (place_id);
      break;
    case Place::DEREF:
      stream << "(";
      stream << "*";
      visit_place (place.path.parent);
      stream << ")";
      break;
    case Place::FIELD:
      stream << "(";
      visit_place (place.path.parent);
      stream << ".";
      stream << place.variable_or_field_index;
      stream << ": " << get_tyty_name (place.tyty) << ")";
      break;
    case Place::INDEX:
      stream << "(";
      visit_place (place.path.parent);
      stream << "[]";
      stream << ": " << get_tyty_name (place.tyty) << ")";
      break;
    case Place::CONSTANT:
      stream << "const " << get_tyty_name (place.tyty);
      break;
    case Place::INVALID:
      stream << "_INVALID";
    }
}

void
Dump::visit_move_place (PlaceId place_id)
{
  const Place &place = place_db[place_id];
  if (!place.is_copy)
    stream << "move ";
  visit_place (place_id);
}

void
Dump::visit (BorrowExpr &expr)
{
  stream << "&";
  visit_lifetime (node_place);
  visit_place (expr.get_place ());
}

void
Dump::visit_lifetime (PlaceId place_id)
{
  const Place &place = place_db[place_id];
  if (place.lifetime.has_lifetime ())
    {
      if (place.lifetime.id == STATIC_LIFETIME_ID)
	stream << "'static ";
      else
	stream << "'#" << get_lifetime_name (place.lifetime) << " ";
    }
}

void
Dump::visit (InitializerExpr &expr)
{
  stream << "{";
  for (auto &place : expr.get_values ())
    {
      visit_move_place (place);
      stream << ", ";
    }
  stream << "}";
}

void
Dump::visit (CallExpr &expr)
{
  stream << "Call(";
  if (auto fn_type
      = place_db[expr.get_callable ()].tyty->try_as<TyTy::FnType> ())
    {
      stream << fn_type->get_identifier ();
    }
  else
    {
      visit_move_place (expr.get_callable ());
    }
  stream << ")(";
  for (auto &place : expr.get_arguments ())
    {
      visit_move_place (place);
      stream << ", ";
    }
  stream << ")";
}

void
Dump::visit (Operator<1> &expr)
{
  stream << "Operator(";
  visit_move_place (expr.get_operand<0> ());
  stream << ")";
}

void
Dump::visit (Operator<2> &expr)
{
  stream << "Operator(";
  visit_move_place (expr.get_operand<0> ());
  stream << ", ";
  visit_move_place (expr.get_operand<1> ());
  stream << ")";
}
void
Dump::visit (Assignment &expr)
{
  visit_move_place (expr.get_rhs ());
}

} // namespace BIR
} // namespace Rust
