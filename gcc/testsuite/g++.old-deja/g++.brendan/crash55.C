// { dg-do compile }
// GROUPS passed old-abort
      extern int f(int); // { dg-error "ambiguates" }

      int& f(int x)  // { dg-error "new declaration" }
      {
          int local; // { dg-error "reference to local" }

          local = x+2;
      
          return local;
      }
