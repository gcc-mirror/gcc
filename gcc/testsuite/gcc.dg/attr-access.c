/* PR c/93640 - The write_only and read_write attributes can be mistyped
   due to invalid strncmp size argument
   { dg-do compile }
   { dg-options "-Wall" } */

__attribute__ ((access (read_onl))) int f0 (char*);         // { dg-error "attribute 'access' invalid mode 'read_onl'" }
__attribute__ ((access (write_onl))) int f1 (char*);        // { dg-error "attribute 'access' invalid mode 'write_onl'" }
__attribute__ ((access (read_writ))) int f2 (char*);        // { dg-error "attribute 'access' invalid mode 'read_writ'" }

__attribute__ ((access (read_onlX))) int f3 (char*);        // { dg-error "attribute 'access' invalid mode 'read_onlX'" }
__attribute__ ((access (write_onlX))) int f4 (char*);       // { dg-error "attribute 'access' invalid mode 'write_onlX'" }
__attribute__ ((access (read_writX))) int f5 (char*);       // { dg-error "attribute 'access' invalid mode 'read_writX'" }


__attribute__ ((access (read_onl, 1))) int f7 (char*);      // { dg-error "attribute 'access' invalid mode 'read_onl'" }
__attribute__ ((access (write_onl, 1))) int f8 (char*);     // { dg-error "attribute 'access' invalid mode 'write_onl'" }
__attribute__ ((access (read_writ, 1))) int f9 (char*);     // { dg-error "attribute 'access' invalid mode 'read_writ'" }

__attribute__ ((access (read_onlX, 1))) int f10 (char*);    // { dg-error "attribute 'access' invalid mode 'read_onlX'" }
__attribute__ ((access (write_onlX, 1))) int f11 (char*);   // { dg-error "attribute 'access' invalid mode 'write_onlX'" }
__attribute__ ((access (read_writX, 1))) int f12 (char*);   // { dg-error "attribute 'access' invalid mode 'read_writX'" }
