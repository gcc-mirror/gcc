/* Test for buffer overrun stringizing named operators longer than 4
   characters.  */
/* { dg-do compile } */

#define str(x) #x
#define var(v,a,b,c,d) static const char v##a##b##c##d[] = str(xor_eq)
#define var1(v,a,b,c)							\
  var(v,a,b,c,0); var(v,a,b,c,1); var(v,a,b,c,2); var(v,a,b,c,3); 	\
  var(v,a,b,c,4); var(v,a,b,c,5); var(v,a,b,c,6); var(v,a,b,c,7); 	\
  var(v,a,b,c,8); var(v,a,b,c,9)
#define var2(v,a,b)						\
  var1(v,a,b,0); var1(v,a,b,1); var1(v,a,b,2); var1(v,a,b,3);	\
  var1(v,a,b,4); var1(v,a,b,5); var1(v,a,b,6); var1(v,a,b,7);	\
  var1(v,a,b,8); var1(v,a,b,9)
#define var3(v,a)					\
  var2(v,a,0); var2(v,a,1); var2(v,a,2); var2(v,a,3);	\
  var2(v,a,4); var2(v,a,5); var2(v,a,6); var2(v,a,7);	\
  var2(v,a,8); var2(v,a,9)

var3(v,0);
var3(v,1);
var3(v,2);
var3(v,3);
var3(v,4);
var3(v,5);
var3(v,6);
var3(v,7);
var3(v,8);
var3(v,9);
