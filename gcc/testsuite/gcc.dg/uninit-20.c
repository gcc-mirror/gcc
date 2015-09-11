/* Spurious uninitialized variable warnings, from gdb */
/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */
struct os { struct o *o; };
struct o { struct o *next; struct os *se; };
void f(struct o *o){
  struct os *s;
  if(o) s = o->se;
  while(o && s == o->se){
    s++; // here `o' is non-zero and thus s is initialized
    s == o->se  // `?' is essential, `if' does not trigger the warning
      ? (o = o->next, o ? s = o->se : 0)
      : 0;
  }
}



