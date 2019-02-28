// { dg-additional-options -fmodules-ts }
#define SEMI ; // { dg-warning "macro expansion" }
import bob SEMI // { dg-message "expansion of macro" }
#define IMPORT import
IMPORT bob ;
