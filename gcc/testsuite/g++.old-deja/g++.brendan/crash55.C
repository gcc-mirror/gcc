// { dg-do assemble  }
// GROUPS passed old-abort
      extern f(int);// { dg-error "" }  ambiguates.*

      int& f(int x)
      {// { dg-error "" }  new declaration.*
          int local;// { dg-error "" }  warning

          local = x+2;
      
          return local;
      }
