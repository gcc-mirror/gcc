// PR c++/20563: ICE (--enable-checking), infinite loop (--disable-checking)
// Origin:       Giovanni Bajo <giovannibajo@libero.it>

// { dg-do compile }

__label__ *l;  // { dg-error "before" }
