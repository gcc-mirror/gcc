/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */

typedef struct _GtkCssStyleProperty GtkCssStyleProperty;

struct _GtkCssStyleProperty
{
  int *initial_value;
  unsigned int id;
  unsigned int inherit :1;
  unsigned int animated :1;
  unsigned int affects_size :1;
  unsigned int affects_font :1;

  int * parse_value;
  int * query_value;
  int * assign_value;
};

void
g_assertion_message_expr (const char *domain,
			  const char *file,
			  int line,
			  const char *func,
			  const char *expr) __attribute__((__noreturn__));

void
g_assertion_message_expr (const char *domain,
			  const char *file,
			  int line,
			  const char *func,
			  const char *expr)
{
  __builtin_abort ();
}
int
get_id (GtkCssStyleProperty *property)
{
  return 1;
}
int
_gtk_css_style_property_get_type ()
{
  return 1;
}

GtkCssStyleProperty *
g_object_new (int object_type,
            const char *first_property_name,
            ...)
{
  return (GtkCssStyleProperty *) __builtin_malloc (sizeof (GtkCssStyleProperty));
}

typedef enum {
  INHERIT = (1 << 0),
  ANIMATED = (1 << 1),
  RESIZE = (1 << 2),
  FONT = (1 << 3)
} GtkStylePropertyFlags;

int t = 0;
void
gtk_css_style_property_register (const char * name,
				 int expected_id,
				 int value_type,
				 int flags,
				 int *parse_value,
				 int *query_value,
				 int *assign_value,
				 int *initial_value)
{
  GtkCssStyleProperty *node;

  do
    {
      if (__builtin_expect (__extension__ (
					   {
					     int _g_boolean_var_;
					     if (initial_value != ((void *)0))
					       _g_boolean_var_ = 1;
					     else
					       _g_boolean_var_ = 0;
					     _g_boolean_var_;
					   }),
			    1))
	;
      else
        g_assertion_message_expr ("Gtk",
				  "gtkcssstylepropertyimpl.c",
				  85,
				  ((const char*) (__PRETTY_FUNCTION__)),
				  "initial_value != NULL");
    } while (0);

  do
    {
      if (__builtin_expect (__extension__ (
					   {
					     int _g_boolean_var_;
					     if (parse_value != ((void *)0))
					       _g_boolean_var_ = 1;
					     else
					       _g_boolean_var_ = 0;
					     _g_boolean_var_;
					   }),
			    1))
	;
      else
	g_assertion_message_expr ("Gtk",
				  "gtkcssstylepropertyimpl.c",
				  86,
				  ((const char*) (__PRETTY_FUNCTION__)),
				  "parse_value != NULL");
    } while (0);

  do
    {
      if (__builtin_expect (__extension__ (
					   {
					     int _g_boolean_var_;
					     if (value_type == ((int) ((1) << (2)))
						 || query_value != ((void *)0))
					       _g_boolean_var_ = 1;
					     else
					       _g_boolean_var_ = 0;
					     _g_boolean_var_;
					   }),
			    1))
	;
      else
	g_assertion_message_expr ("Gtk",
				  "gtkcssstylepropertyimpl.c",
				  87, ((const char*) (__PRETTY_FUNCTION__)),
				  "value_type == NONE || query_value != NULL");
    } while (0);

  /* FLAGS is changed in a cond_exec instruction with pr57637.  */
  if (flags  == 15)
    t = 15;

  do
    {
      if (__builtin_expect (__extension__ (
					   {
					     int _g_boolean_var_;
					     if (value_type == ((1) << (2))
						 || assign_value != ((void *)0))
					       _g_boolean_var_ = 1;
					     else
					       _g_boolean_var_ = 0;
					     _g_boolean_var_;
					   }),
			    1))
	;
      else
	g_assertion_message_expr ("Gtk",
				  "gtkcssstylepropertyimpl.c",
				  88, ((const char*) (__PRETTY_FUNCTION__)),
				  "value_type == NONE || assign_value != NULL");
    } while (0);

  node = g_object_new ((_gtk_css_style_property_get_type ()),
			"value-type", value_type,
			"affects-size", (flags & RESIZE) ? (0) : (!(0)),
			"affects-font", (flags & FONT) ? (!(0)) : (0),
			"animated", (flags & ANIMATED) ? (!(0)) : (0),
			"inherit", (flags & INHERIT) ? (!(0)) : (0),
			"initial-value", initial_value,
			"name", name,
			((void *)0));

  node->parse_value = parse_value;
  node->query_value = query_value;
  node->assign_value = assign_value;

  do
    {
      if (__builtin_expect (__extension__ (
					   {
					     int _g_boolean_var_;
					     if (get_id (node) == expected_id)
					       _g_boolean_var_ = 1;
					     else
					       _g_boolean_var_ = 0;
					     _g_boolean_var_;
					   }),
			    1))
	;
      else
	g_assertion_message_expr ("Gtk",
				  "gtkcssstylepropertyimpl.c",
				  106,
				  ((const char*) (__PRETTY_FUNCTION__)),
				  "get_id (node) == expected_id");
    } while (0);
}

int main ()
{
  gtk_css_style_property_register ("test", 1, 4, 15, &t, &t, &t, &t);

  if (t != 15)
    __builtin_abort ();
  return 0;
}
