// { dg-do assemble  }
char *stuff() {
   char array[10]; // { dg-warning "" } 

   return array;
}
