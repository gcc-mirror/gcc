// { dg-do assemble  }
// GROUPS passed static
class foo{
public:
  static void  bar( int i ){ value = i; }
  static int  value;// { dg-error "" } .*
};

const int  foo::value = 0; // should be an error.// { dg-error "" } .*

int main(){
  foo::bar( 1 );
  return 0;
}

