/* PR preprocessor/57580 */
/* { dg-do compile } */
/* { dg-options "-save-temps" } */

#define MSG 	\
  _Pragma("message(\"message0\")")	\
  _Pragma("message(\"message1\")")
MSG	/* { dg-message "message0" } */
/* { dg-message "message1" "" { target *-*-* } .-1 } */
