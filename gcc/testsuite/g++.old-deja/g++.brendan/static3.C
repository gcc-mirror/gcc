// Build don't link: 
// GROUPS passed static
class foo{
public:
  static void  bar( int i ){ value = i; }
  static int  value;// ERROR - .*
};

const int  foo::value = 0; // should be an error.// ERROR - .*

int main(){
  foo::bar( 1 );
  return 0;
}

