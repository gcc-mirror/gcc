// { dg-do assemble  }
// GROUPS passed old-abort
        template <class TP> class sapp { };
        class foo {};
        extern foo& __iomanip_setw (foo&, TP);// { dg-error "" }  type spec.*
