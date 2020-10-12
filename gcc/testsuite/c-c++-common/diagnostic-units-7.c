/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-column-unit=byte -fshow-column -fdiagnostics-show-caret -ftabstop=9 -Wmultichar" } */

/* column units: bytes (via arg)
   column origin: 1 (via default)
   tabstop: 9 (via arg) */

/* This line starts with a tab.  */
	int c1 = 'c1'; /* { dg-warning "11: multi-character character constant" } */
/* { dg-begin-multiline-output "" }
          int c1 = 'c1';
                   ^~~~
   { dg-end-multiline-output "" } */

/* This line starts with <tabstop> spaces.  */
         int c2 = 'c2'; /* { dg-warning "19: multi-character character constant" } */
/* { dg-begin-multiline-output "" }
          int c2 = 'c2';
                   ^~~~
   { dg-end-multiline-output "" } */

/* This line starts with <tabstop> spaces and has an internal tab after
   a space.  */
         int c3 = 	'c3'; /* { dg-warning "20: multi-character character constant" } */
/* { dg-begin-multiline-output "" }
          int c3 =          'c3';
                            ^~~~
   { dg-end-multiline-output "" } */
