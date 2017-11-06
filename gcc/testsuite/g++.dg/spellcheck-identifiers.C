/* { dg-do compile } */
/* { dg-options "-fdiagnostics-show-caret" } */

typedef struct GtkWidget { int dummy; } GtkWidget;

extern void gtk_widget_show_all (GtkWidget *w);


void
test_1 (GtkWidget *w)
{
  gtk_widget_showall (w); // { dg-error "3: 'gtk_widget_showall' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   gtk_widget_showall (w);
   ^~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "3: suggested alternative: 'gtk_widget_show_all'" "" { target *-*-* } 12 }
  /* { dg-begin-multiline-output "" }
   gtk_widget_showall (w);
   ^~~~~~~~~~~~~~~~~~
   gtk_widget_show_all
   { dg-end-multiline-output "" } */

  /* Ensure we don't try to suggest "gtk_widget_showall" for subsequent
     corrections.  */
  gtk_widget_showall_ (w); // { dg-error "3: 'gtk_widget_showall_' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   gtk_widget_showall_ (w);
   ^~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "3: suggested alternative: 'gtk_widget_show_all'" "" { target *-*-* } 26 }
  /* { dg-begin-multiline-output "" }
   gtk_widget_showall_ (w);
   ^~~~~~~~~~~~~~~~~~~
   gtk_widget_show_all
   { dg-end-multiline-output "" } */

  GtkWidgetShowAll (w); // { dg-error "3: 'GtkWidgetShowAll' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   GtkWidgetShowAll (w);
   ^~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "3: suggested alternative: 'gtk_widget_show_all'" "" { target *-*-* } 38 }
  /* { dg-begin-multiline-output "" }
   GtkWidgetShowAll (w);
   ^~~~~~~~~~~~~~~~
   gtk_widget_show_all
   { dg-end-multiline-output "" } */
}

int
test_2 (int param)
{
  return parma * parma; // { dg-error "10: 'parma' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   return parma * parma;
          ^~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "10: suggested alternative: 'param'" "" { target *-*-* } 54 }
  /* { dg-begin-multiline-output "" }
   return parma * parma;
          ^~~~~
          param
   { dg-end-multiline-output "" } */
}

#define MACRO(X) ((X))

int
test_3 (int i)
{
  return MACRAME (i); // { dg-error "10: 'MACRAME' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   return MACRAME (i);
          ^~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "10: suggested alternative: 'MACRO'" "" { target *-*-* } 72 }
  /* { dg-begin-multiline-output "" }
   return MACRAME (i);
          ^~~~~~~
          MACRO
   { dg-end-multiline-output "" } */
}

#define IDENTIFIER_POINTER(X) ((X))

int
test_4 (int node)
{
  return IDENTIFIER_PTR (node); // { dg-error "10: 'IDENTIFIER_PTR' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   return IDENTIFIER_PTR (node);
          ^~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "10: suggested alternative: 'IDENTIFIER_POINTER'" "" { target *-*-* } 90 }
  /* { dg-begin-multiline-output "" }
   return IDENTIFIER_PTR (node);
          ^~~~~~~~~~~~~~
          IDENTIFIER_POINTER
   { dg-end-multiline-output "" } */
}


int
test_5 (void)
{
  return __LINE_; /* { dg-error "10: '__LINE_' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   return __LINE_;
          ^~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "10: suggested alternative: '__LINE__'" "" { target *-*-* } 107 }
  /* { dg-begin-multiline-output "" }
   return __LINE_;
          ^~~~~~~
          __LINE__
   { dg-end-multiline-output "" } */
}

#define MAX_ITEMS 100
int array[MAX_ITEM]; // { dg-error "11: 'MAX_ITEM' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
 int array[MAX_ITEM];
           ^~~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "11: suggested alternative: 'MAX_ITEMS'" "" { target *-*-* } 121 }
  /* { dg-begin-multiline-output "" }
 int array[MAX_ITEM];
           ^~~~~~~~
           MAX_ITEMS
   { dg-end-multiline-output "" } */


enum foo {
  FOO_FIRST,
  FOO_SECOND
};

void
test_6 (enum foo f)
{
  switch (f)
    {
    case FOO_FURST: // { dg-error "10: 'FOO_FURST' was not declared in this scope" }
      break;
  /* { dg-begin-multiline-output "" }
     case FOO_FURST:
          ^~~~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "10: suggested alternative: 'FOO_FIRST'" "" { target *-*-* } 144 }
  /* { dg-begin-multiline-output "" }
     case FOO_FURST:
          ^~~~~~~~~
          FOO_FIRST
   { dg-end-multiline-output "" } */

    case FOO_SECCOND: // { dg-error "10: 'FOO_SECCOND' was not declared in this scope" }
      break;
  /* { dg-begin-multiline-output "" }
     case FOO_SECCOND:
          ^~~~~~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "10: suggested alternative: 'FOO_SECOND'" "" { target *-*-* } 157 }
  /* { dg-begin-multiline-output "" }
     case FOO_SECCOND:
          ^~~~~~~~~~~
          FOO_SECOND
   { dg-end-multiline-output "" } */

    default:
      break;
    }
}

int snprintf (char *, __SIZE_TYPE__, const char *, ...);

void
test_7 (int i, int j)
{
  int buffer[100];
  snprint (buffer, 100, "%i of %i", i, j); // { dg-error "3: 'snprint' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   snprint (buffer, 100, "%i of %i", i, j);
   ^~~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "3: suggested alternative: 'snprintf'" "" { target *-*-* } 181 }
  /* { dg-begin-multiline-output "" }
   snprint (buffer, 100, "%i of %i", i, j);
   ^~~~~~~
   snprintf
   { dg-end-multiline-output "" } */
}

int
test_8 ()
{
  int local = 42;
  
  return locale; // { dg-error "10: 'locale' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   return locale;
          ^~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "10: suggested alternative: 'local'" "" { target *-*-* } 199 }
  /* { dg-begin-multiline-output "" }
   return locale;
          ^~~~~~
          local
   { dg-end-multiline-output "" } */
}

class base
{
public:
  int test_method_1 ();

protected:
  int m_foo;
};

class sub : public base
{
public:
  int test_method_2 ();
};

int base::test_method_1 ()
{
  return m_food; // { dg-error "10: 'm_food' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   return m_food;
          ^~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "10: suggested alternative: 'm_foo'" "" { target *-*-* } 229 }
  /* { dg-begin-multiline-output "" }
   return m_food;
          ^~~~~~
          m_foo
   { dg-end-multiline-output "" } */
}

int sub::test_method_2 ()
{
  return m_food; // { dg-error "10: 'm_food' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   return m_food;
          ^~~~~~
   { dg-end-multiline-output "" } */
  // { dg-message "10: suggested alternative: 'm_foo'" "" { target *-*-* } 244 }
  /* { dg-begin-multiline-output "" }
   return m_food;
          ^~~~~~
          m_foo
   { dg-end-multiline-output "" } */
}
