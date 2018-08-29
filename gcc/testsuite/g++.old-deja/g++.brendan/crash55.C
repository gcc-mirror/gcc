// { dg-do compile }
// GROUPS passed old-abort
      extern int f(int); // { dg-message "old declaration" }

      int& f(int x)  // { dg-error "new declaration" }
      {
          int local;

          local = x+2;
      
          return local; // { dg-warning "reference to local" }
      }
