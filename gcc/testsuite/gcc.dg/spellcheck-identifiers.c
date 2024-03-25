/* { dg-do compile } */
/* { dg-options "-fpermissive -fdiagnostics-show-caret" } */

typedef struct GtkWidget { int dummy; } GtkWidget;

extern void gtk_widget_show_all (GtkWidget *w);

void
test_1 (GtkWidget *w)
{
  gtk_widget_showall (w); /* { dg-warning "3: implicit declaration of function .gtk_widget_showall.; did you mean .gtk_widget_show_all.?" } */
  /* { dg-begin-multiline-output "" }
   gtk_widget_showall (w);
   ^~~~~~~~~~~~~~~~~~
   gtk_widget_show_all
   { dg-end-multiline-output "" } */

  /* Ensure we don't try to suggest "gtk_widget_showall" for subsequent
     corrections.  */
  gtk_widget_showall_ (w); /* { dg-warning "3: implicit declaration of function .gtk_widget_showall_.; did you mean .gtk_widget_show_all.?" } */
  /* { dg-begin-multiline-output "" }
   gtk_widget_showall_ (w);
   ^~~~~~~~~~~~~~~~~~~
   gtk_widget_show_all
   { dg-end-multiline-output "" } */

  GtkWidgetShowAll (w); /* { dg-warning "3: implicit declaration of function .GtkWidgetShowAll.; did you mean .gtk_widget_show_all.?" } */
  /* { dg-begin-multiline-output "" }
   GtkWidgetShowAll (w);
   ^~~~~~~~~~~~~~~~
   gtk_widget_show_all
   { dg-end-multiline-output "" } */
}

int
test_2 (int param)
{
  return parma * parma; /* { dg-error "10: .parma. undeclared .first use in this function.; did you mean .param." } */
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
  return MACRAME (i); /* { dg-warning "10: implicit declaration of function .MACRAME.; did you mean .MACRO.?" } */
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
  return IDENTIFIER_PTR (node); /* { dg-warning "10: implicit declaration of function .IDENTIFIER_PTR.; did you mean .IDENTIFIER_POINTER.?" } */
  /* { dg-begin-multiline-output "" }
   return IDENTIFIER_PTR (node);
          ^~~~~~~~~~~~~~
          IDENTIFIER_POINTER
   { dg-end-multiline-output "" } */
}


int
test_5 (void)
{
  return __LINE_; /* { dg-error "10: .__LINE_. undeclared .first use in this function.; did you mean .__LINE__." } */
  /* { dg-begin-multiline-output "" }
   return __LINE_;
          ^~~~~~~
          __LINE__
   { dg-end-multiline-output "" } */
}

#define MAX_ITEMS 100
int array[MAX_ITEM]; /* { dg-error "11: .MAX_ITEM. undeclared here .not in a function.; did you mean .MAX_ITEMS." } */
  /* { dg-begin-multiline-output "" }
 int array[MAX_ITEM];
           ^~~~~~~~
           MAX_ITEMS
   { dg-end-multiline-output "" } */


enum foo {
  FOO_FIRST,
  FOO_SECOND
};

int
test_6 (enum foo f)
{
  switch (f)
    {
    case FOO_FURST: /* { dg-error "10: .FOO_FURST. undeclared .first use in this function.; did you mean .FOO_FIRST." } */
      break;
  /* { dg-begin-multiline-output "" }
     case FOO_FURST:
          ^~~~~~~~~
          FOO_FIRST
   { dg-end-multiline-output "" } */

    case FOO_SECCOND: /* { dg-error "10: .FOO_SECCOND. undeclared .first use in this function.; did you mean .FOO_SECOND." } */
      break;
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
  snprint (buffer, 100, "%i of %i", i, j); /* { dg-warning "3: implicit declaration of function .snprint.; did you mean .snprintf.." } */
  /* { dg-begin-multiline-output "" }
   snprint (buffer, 100, "%i of %i", i, j);
   ^~~~~~~
   snprintf
   { dg-end-multiline-output "" } */
}
