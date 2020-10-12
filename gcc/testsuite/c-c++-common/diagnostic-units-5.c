/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-column-unit=display -fshow-column -fdiagnostics-show-caret -fdiagnostics-column-origin=0 -Wmultichar" } */

/* column units: display (via arg)
   column origin: 0 (via arg)
   tabstop: 8 (via default) */

/* This line starts with a tab.  */
	int c1 = 'c1'; /* { dg-warning "17: multi-character character constant" } */
/* { dg-begin-multiline-output "" }
         int c1 = 'c1';
                  ^~~~
   { dg-end-multiline-output "" } */

/* This line starts with <tabstop> spaces.  */
        int c2 = 'c2'; /* { dg-warning "17: multi-character character constant" } */
/* { dg-begin-multiline-output "" }
         int c2 = 'c2';
                  ^~~~
   { dg-end-multiline-output "" } */

/* This line starts with <tabstop> spaces and has an internal tab after
   a space.  */
        int c3 = 	'c3'; /* { dg-warning "24: multi-character character constant" } */
/* { dg-begin-multiline-output "" }
         int c3 =        'c3';
                         ^~~~
   { dg-end-multiline-output "" } */
