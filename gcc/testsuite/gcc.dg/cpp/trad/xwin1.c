/* XWindows (as of 4.3) does some pretty strange things with cpp.
   This tests one of them; the leading comments are supposed to be
   eaten by the preprocessor; but the 'directives' after them are
   supposed to be retained as text, not processed, so that imake's cpp
   can be run on the output!
   { dg-do preprocess }
*/

/**/#if 0
passed
/**/#endif

/* { dg-final { scan-file xwin1.i "(^|\n)#if 0" } } */
