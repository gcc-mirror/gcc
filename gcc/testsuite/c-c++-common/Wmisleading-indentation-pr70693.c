/* { dg-options "-Wmisleading-indentation" } */

int in_what; /*        */

void process_char(char c) {
    switch( 0 ) {
      case 0:
      	if( c == '>' ) in_what = 0;
      	break;
    
    }
}
