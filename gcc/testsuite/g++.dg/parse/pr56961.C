// PR c++/56961

struct foo { };

typedef struct
{
  volatile foo fields;
} CSPHandleState;
 
CSPHandleState a;

void fn1 ()
{
  CSPHandleState b;
  b.fields = foo();  // { dg-error "discards qualifiers" }
}
