/* PR c++/101940 */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-Wno-attributes=company::,yoyodyne::attr" } */
/* { dg-additional-options "-Wno-attributes=c1::attr,c1::attr,c1::__attr__" } */
/* { dg-additional-options "-Wno-attributes=c2::,c2::attr" } */
/* { dg-additional-options "-Wno-attributes=c3::attr,c3::" } */
/* { dg-additional-options "-Wno-attributes=x::" } */
/* { dg-additional-options "-Wno-attributes=yoyodyne::attr_new" } */
/* { dg-additional-options "-Wno-attributes=c4::__attr__" } */
/* { dg-additional-options "-Wno-attributes=c5::attr" } */
/* { dg-additional-options "-Wno-attributes=__c6__::attr" } */

[[company::attr]] void f1();
[[company::attr2]] void f2();

[[yoyodyne::attr]] void f3();
[[yoyodyne::__attr__]] void f3__();
[[yoyodyne::attrx]] void f4(); /* { dg-warning "ignored" } */
[[yoyodyne::__attrx__]] void f4__(); /* { dg-warning "ignored" } */

[[c1::attr]] void f5();

[[c2::attr]] void f6();
[[c2::attrx]] void f7();
[[c2::__attr__]] void f6__();
[[c2::__attrx__]] void f7__();

[[c3::attr]] void f8();
[[c3::attrx]] void f9();

[[x::x]] void f10();

[[yoyodyne::attr_new]] void f11();
[[yoyodyne::__attr_new__]] void f11__();
[[yoyodyne::attr_mew]] void f12(); /* { dg-warning "ignored" } */
[[yoyodyne::__attr_mew__]] void f12__(); /* { dg-warning "ignored" } */

[[c4::attr]] void f13();
[[c4::__attr__]] void f13__();
[[c4::attrx]] void f14(); /* { dg-warning "ignored" } */

[[c5::attr]] void f15();
[[c5::__attr__]] void f15__();
[[__c5__::attr]] void __f15();
[[__c5__::__attr__]] void __f15__();
[[c5::attrx]] void f15x(); /* { dg-warning "ignored" } */
[[__c5__::attrx]] void f15x(); /* { dg-warning "ignored" } */

[[c6::attr]] void f16();
[[c6::__attr__]] void f16__();
[[__c6__::attr]] void __f16();
[[__c6__::__attr__]] void __f16__();
[[c6::attrx]] void f16x(); /* { dg-warning "ignored" } */
[[__c6__::attrx]] void f16x(); /* { dg-warning "ignored" } */
