/* PR c++/33207  */

/* This would not ICE.  */
namespace M { } /* { dg-message "previous declaration" } */
struct M; /* { dg-error "redeclared as different kind of symbol" } */
M *p; /* { dg-error "does not name a type" } */

/* This would ICE when processing 'p'.  */
namespace N { } /* { dg-message "previous declaration" } */
struct N; /* { dg-error "redeclared as different kind of symbol" } */
struct N* p; /* { dg-error "redeclared as different kind of symbol|invalid type" } */
