// { dg-do assemble  }
char *stuff() {
   char array[10];

   return array;		// { dg-warning "" } 
}
