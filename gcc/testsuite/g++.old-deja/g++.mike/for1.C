// { dg-do assemble  }
int main() {
  for( {int i = 0; int j = 0;} i < 10; ++i ) ;		// { dg-error "" } 
}
