/* __thread specifiers on empty declarations.  */

__thread struct foo; /* { dg-warning "warning: useless '__thread' in empty declaration" } */
