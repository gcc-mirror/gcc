/* { dg-options "-O" } */

/* This plugin exercises the text_art code.  */

#define INCLUDE_VECTOR
#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "diagnostic-diagram.h"
#include "text-art/canvas.h"
#include "text-art/table.h"

int plugin_is_GPL_compatible;

using namespace text_art;

/* Canvas tests.  */

static void
emit_canvas (const canvas &c, const char *alt_text)
{
  diagnostic_diagram diagram (c, alt_text);
  global_dc->emit_diagram (diagram);
}

static void
test_abc ()
{
  style_manager sm;
  canvas c (canvas::size_t (3, 3), sm);
  c.paint (canvas::coord_t (0, 0), styled_unichar ('A'));
  c.paint (canvas::coord_t (1, 1), styled_unichar ('B'));
  c.paint (canvas::coord_t (2, 2), styled_unichar ('C'));
  emit_canvas (c, "test_abc");
}

/* Test of procedural art using 24-bit color: chess starting position.  */

static void
test_chessboard ()
{
  /* With the exception of NONE, these are in order of the chess symbols
     in the Unicode Miscellaneous Symbols block.  */
  enum class piece { KING, QUEEN, ROOK, BISHOP, KNIGHT, PAWN, NONE };
  enum class color { BLACK, WHITE, NONE };

  style_manager sm;

  /* We assume double-column chars for the pieces, so allow two canvas
     columns per square.  */
  canvas canvas (canvas::size_t (16, 8), sm);

  for (int x = 0; x < 8; x++)
    for (int y = 0; y < 8; y++)
      {
	enum piece piece_kind;
	enum color piece_color;
	switch (y)
	  {
	  case 0:
	  case 7:
	    switch (x)
	      {
	      default:
		gcc_unreachable ();
	      case 0:
		piece_kind = piece::ROOK;
		break;
	      case 1:
		piece_kind = piece::KNIGHT;
		break;
	      case 2:
		piece_kind = piece::BISHOP;
		break;
	      case 3:
		piece_kind = piece::QUEEN;
		break;
	      case 4:
		piece_kind = piece::KING;
		break;
	      case 5:
		piece_kind = piece::BISHOP;
		break;
	      case 6:
		piece_kind = piece::KNIGHT;
		break;
	      case 7:
		piece_kind = piece::ROOK;
	      break;
	      }
	    piece_color = (y == 0) ? color::BLACK : color::WHITE;
	    break;
	  case 1:
	  case 6:
	    piece_kind = piece::PAWN;
	    piece_color = (y == 1) ? color::BLACK : color::WHITE;
	    break;
	  default:
	    piece_kind = piece::NONE;
	    piece_color = color::NONE;
	    break;
	  }

	style s;
	const bool white_square = (x + y) % 2 == 0;
	if (white_square)
	  s.m_bg_color = style::color (0xf0, 0xd9, 0xb5);
	else
	  s.m_bg_color = style::color (0xb5, 0x88, 0x63);
	switch (piece_color)
	  {
	  default:
	    gcc_unreachable ();
	  case color::WHITE:
	    s.m_fg_color = style::color (0xff, 0xff, 0xff);
	    break;
	  case color::BLACK:
	    s.m_fg_color = style::color (0x00, 0x00, 0x00);
	    break;
	  case color::NONE:
	    break;
	  }
	style::id_t style_id = sm.get_or_create_id (s);

	cppchar_t ch;
	if (piece_kind == piece::NONE)
	  ch = ' ';
	else
	  {
	    const cppchar_t WHITE_KING = 0x2654;
	    const cppchar_t BLACK_KING = 0x265A;
	    cppchar_t base ((piece_color == color::WHITE)
			    ? WHITE_KING : BLACK_KING);
	    ch = base + ((int)piece_kind - (int)piece::KING);
	  }
	canvas.paint (canvas::coord_t (x * 2, y),
		      canvas::cell_t (ch, false, style_id));
	canvas.paint (canvas::coord_t (x * 2 + 1, y),
		      canvas::cell_t (' ', false, style_id));
      }
  emit_canvas (canvas, "test_chessboard");
}

/* Table tests.  */

static void
emit_table (const table &table, const style_manager &sm, const char *alt_text)
{
  const text_art::theme *theme = global_dc->get_diagram_theme ();
  if (!theme)
    return;
  canvas c (table.to_canvas (*theme, sm));
  emit_canvas (c, alt_text);
}

static void
test_double_width_chars ()
{
  style_manager sm;
  table table (table::size_t (1, 1));
  table.set_cell (table::coord_t (0,0),
		  styled_string ((cppchar_t)0x1f642));

  emit_table (table, sm, "test_double_width_chars");
}

static void
test_ipv4_header ()
{
  style_manager sm;
  table table (table::size_t (34, 10));
  table.set_cell (table::coord_t (0, 0), styled_string (sm, "Offsets"));
  table.set_cell (table::coord_t (1, 0), styled_string (sm, "Octet"));
  table.set_cell (table::coord_t (0, 1), styled_string (sm, "Octet"));
  for (int octet = 0; octet < 4; octet++)
    table.set_cell_span (table::rect_t (table::coord_t (2 + (octet * 8), 0),
					table::size_t (8, 1)),
			 styled_string::from_fmt (sm, nullptr, "%i", octet));
  table.set_cell (table::coord_t (1, 1), styled_string (sm, "Bit"));
  for (int bit = 0; bit < 32; bit++)
    table.set_cell (table::coord_t (bit + 2, 1),
		    styled_string::from_fmt (sm, nullptr, "%i", bit));
  for (int word = 0; word < 6; word++)
    {
      table.set_cell (table::coord_t (0, word + 2),
		      styled_string::from_fmt (sm, nullptr, "%i", word * 4));
      table.set_cell (table::coord_t (1, word + 2),
		      styled_string::from_fmt (sm, nullptr, "%i", word * 32));
    }

  table.set_cell (table::coord_t (0, 8), styled_string (sm, "..."));
  table.set_cell (table::coord_t (1, 8), styled_string (sm, "..."));
  table.set_cell (table::coord_t (0, 9), styled_string (sm, "56"));
  table.set_cell (table::coord_t (1, 9), styled_string (sm, "448"));

#define SET_BITS(FIRST, LAST, NAME)					\
  do {									\
    const int first = (FIRST);						\
    const int last = (LAST);						\
    const char *name = (NAME);						\
    const int row = first / 32;						\
    gcc_assert (last / 32 == row);					\
    table::rect_t rect (table::coord_t ((first % 32) + 2, row + 2),	\
			table::size_t (last + 1 - first , 1));		\
    table.set_cell_span (rect, styled_string (sm, name));		\
  } while (0)

  SET_BITS (0, 3, "Version");
  SET_BITS (4, 7, "IHL");
  SET_BITS (8, 13, "DSCP");
  SET_BITS (14, 15, "ECN");
  SET_BITS (16, 31, "Total Length");

  SET_BITS (32 +  0, 32 + 15, "Identification");
  SET_BITS (32 + 16, 32 + 18, "Flags");
  SET_BITS (32 + 19, 32 + 31, "Fragment Offset");

  SET_BITS (64 +  0, 64 +  7, "Time To Live");
  SET_BITS (64 +  8, 64 + 15, "Protocol");
  SET_BITS (64 + 16, 64 + 31, "Header Checksum");

  SET_BITS (96 +  0, 96 + 31, "Source IP Address");
  SET_BITS (128 +  0, 128 + 31, "Destination IP Address");

  table.set_cell_span(table::rect_t (table::coord_t (2, 7),
				     table::size_t (32, 3)),
		      styled_string (sm, "Options"));

  emit_table (table, sm, "test_ipv4_header");
}

static void
show_diagrams ()
{
  test_abc ();
  test_chessboard ();
  test_double_width_chars ();
  test_ipv4_header ();
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  struct plugin_argument *argv = plugin_info->argv;

  if (!plugin_default_version_check (version, &gcc_version))
    return 1;

  show_diagrams ();

  return 0;
}
