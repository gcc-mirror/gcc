// Test of -Wformat for subclasses of pp_element
// { dg-do compile }
// { dg-options "-Wformat" }

union tree_node;
typedef union tree_node *tree;
class rich_location;
class pretty_printer;

namespace pp_markup {
class element {};

} // namespace pp_markup

typedef pp_markup::element pp_element;

extern void error_at (rich_location *, const char *, ...)
  __attribute__ ((__format__ (__gcc_cdiag__, 2, 3),
		  __nonnull__ (2)));

namespace highlight_colors {

extern const char *const lhs;
extern const char *const rhs;

} // namespace highlight_colors

namespace pp_markup {

class element_quoted_type : public element
{
public:
  element_quoted_type (tree type, const char *highlight_color)
  : m_type (type),
    m_highlight_color (highlight_color)
  {
  }
private:
  tree m_type;
  const char *m_highlight_color;
};

}

void
binary_op_error (rich_location *richloc, const char *opname,
		 tree type0, tree type1)
{
  pp_markup::element_quoted_type element_0 (type0, highlight_colors::lhs);
  pp_markup::element_quoted_type element_1 (type1, highlight_colors::rhs);
  error_at (richloc,
	    "invalid operands to binary %s (have %e and %e)",
	    opname, &element_0, &element_1);
}
