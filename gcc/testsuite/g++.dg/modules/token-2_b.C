// { dg-additional-options -fmodules-ts }
#define SEMI ; // { dg-error "macro expansion" }
import bob SEMI // { dg-message "expansion of macro" }
#define IMPORT import
IMPORT bob ;
