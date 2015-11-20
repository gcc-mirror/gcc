/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret" } */

/* This is an example file for use with
   diagnostic_plugin_show_trees.c.

   The plugin handles "__show_tree" by recursively dumping
   the internal structure of the second input argument.

   We want to accept an expression of any type.  To do this in C, we
   use variadic arguments, but C requires at least one argument before
   the ellipsis, so we have a dummy one.  */

extern void __show_tree (int dummy, ...);

extern double sqrt (double x);

void test_quadratic (double a, double b, double c)
{
  __show_tree (0,
     (-b + sqrt (b * b - 4 * a * c))
     / (2 * a));

/* { dg-begin-multiline-output "" }
      (-b + sqrt (b * b - 4 * a * c))
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      / (2 * a));
      ^~~~~~~~~
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
      (-b + sqrt (b * b - 4 * a * c))
      ~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
      (-b + sqrt (b * b - 4 * a * c))
            ^~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
      (-b + sqrt (b * b - 4 * a * c))
                  ~~~~~~^~~~~~~~~~~
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
      (-b + sqrt (b * b - 4 * a * c))
                  ~~^~~
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
      (-b + sqrt (b * b - 4 * a * c))
                          ~~~~~~^~~
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
      (-b + sqrt (b * b - 4 * a * c))
                          ~~^~~
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
      / (2 * a));
        ~~~^~~~
   { dg-end-multiline-output "" } */
}
