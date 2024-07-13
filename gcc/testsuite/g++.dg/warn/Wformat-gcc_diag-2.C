// Test of -Wformat for subclasses of pp_element
// { dg-do compile }
// { dg-options "-Wformat" }

/* Magic identifiers must be set before the attribute is used.  */

typedef long long __gcc_host_wide_int__;

typedef struct location_s
{
  const char *file;
  int line;
} location_t;

union tree_node;
typedef union tree_node *tree;

/* Define gimple as a dummy type.  The typedef must be provided for
   the C test to find the symbol.  */
typedef struct gimple gimple;

/* Likewise for gimple.  */
typedef struct cgraph_node cgraph_node;

/* Likewise for diagnostic_event_id_t.  */
typedef struct diagnostic_event_id_t diagnostic_event_id_t;

namespace pp_markup {
class element
{
};
} // namespace pp_markup
typedef pp_markup::element pp_element;

#define FORMAT(kind) __attribute__ ((format (__gcc_## kind ##__, 1, 2)))

void diag (const char*, ...) FORMAT (diag);

class sub_element : public pp_element
{
};

class sub_sub_element : public sub_element
{
};

void test_diag ()
{
  sub_element e1;
  sub_sub_element e2;
  diag ("%e %e", &e1, &e2);
}
