// Origin: PR c++/51145
// { dg-do compile { target c++11 } }

struct A {};

template<class>
using X = A;

struct X<int>* px; // { dg-error "using\[^\n\r\]*alias\[^\n\r\]*specialization\[^\n\r\]*X<int>\[^\n\r\]*after\[^\n\r\]*struct|invalid type in declaration before\[^\n\r\]*;" }

template<int>
using Y = A;

struct Y<0>* py;// { dg-error "alias\[^\n\r\]*specialization\[^\n\r\]*Y<0>\[^\n\r\]*after\[^\n\r\]*struct|invalid type in declaration before\[^\n\r\]*;" }
