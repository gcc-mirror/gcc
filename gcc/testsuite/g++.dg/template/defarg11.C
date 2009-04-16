// { dg-do compile }

// We used to reject this code as the extension
// for default arguments being accepted as less the
// needed template arguments.


template<typename> struct match { }; 
 
template<template<typename> class t,typename T>  
struct match<t<T> > { typedef int type; }; 
 
template<template<typename,typename> class t,typename T0,typename T1> 
struct match<t<T0,T1> > { typedef int type; }; 
 
template<typename,typename =void> struct other { }; 
 
typedef match<other<void,void> >::type type; 
