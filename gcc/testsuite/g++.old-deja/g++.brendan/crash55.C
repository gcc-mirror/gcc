// Build don't link: 
// GROUPS passed old-abort
      extern f(int);// ERROR -  ambiguates.*

      int& f(int x)
      {// ERROR -  new declaration.*
          int local;// ERROR -  warning

          local = x+2;
      
          return local;
      }
