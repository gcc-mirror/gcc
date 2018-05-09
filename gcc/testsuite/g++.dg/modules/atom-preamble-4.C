// { dg-additional-options -fmodules-atom }

#define NAME(X) X; // { dg-message "ends inside macro" }

export module NAME(bob)

