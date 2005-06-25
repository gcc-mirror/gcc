/* Copyright 1996, 1999 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

   Please email any bugs, comments, and/or additions to this file to:
   bug-gdb@prep.ai.mit.edu  */

struct struct1 { char a;};
struct struct2 { char a, b;};
struct struct3 { char a, b, c; };
struct struct4 { char a, b, c, d; };
struct struct5 { char a, b, c, d, e; };
struct struct6 { char a, b, c, d, e, f; };
struct struct7 { char a, b, c, d, e, f, g; };
struct struct8 { char a, b, c, d, e, f, g, h; };
struct struct9 { char a, b, c, d, e, f, g, h, i; };
struct struct10 { char a, b, c, d, e, f, g, h, i, j; };
struct struct11 { char a, b, c, d, e, f, g, h, i, j, k; };
struct struct12 { char a, b, c, d, e, f, g, h, i, j, k, l; };
struct struct16 { char a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p; };

struct struct1 foo1 = {'1'},  L1;
struct struct2 foo2 = { 'a', 'b'},  L2;
struct struct3 foo3 = { 'A', 'B', 'C'},  L3;
struct struct4 foo4 = {'1', '2', '3', '4'},  L4;
struct struct5 foo5 = {'a', 'b', 'c', 'd', 'e'},  L5;
struct struct6 foo6 = {'A', 'B', 'C', 'D', 'E', 'F'},  L6;
struct struct7 foo7 = {'1', '2', '3', '4', '5', '6', '7'},  L7;
struct struct8 foo8 = {'1', '2', '3', '4', '5', '6', '7', '8'},  L8;
struct struct9 foo9 = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'},  L9;
struct struct10 foo10 = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'},  L10;
struct struct11 foo11 = {
  '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B'}, L11;
struct struct12 foo12 = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L'}, L12;
struct struct16 foo16 = {
  'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p'}, L16;

struct struct1  fun1()
{
  return foo1;  
}
struct struct2  fun2()
{
  return foo2;
}
struct struct3  fun3()
{
  return foo3;
}
struct struct4  fun4()
{
  return foo4;
}
struct struct5  fun5()
{
  return foo5;
}
struct struct6  fun6()
{
  return foo6;
}
struct struct7  fun7()
{
  return foo7;
}
struct struct8  fun8()
{
  return foo8;
}
struct struct9  fun9()
{
  return foo9;
}
struct struct10 fun10()
{
  return foo10; 
}
struct struct11 fun11()
{
  return foo11; 
}
struct struct12 fun12()
{
  return foo12; 
}
struct struct16 fun16()
{
  return foo16; 
}

#ifdef PROTOTYPES
void Fun1(struct struct1 foo1)
#else
void Fun1(foo1)
     struct struct1 foo1;
#endif
{
  L1 = foo1;
}
#ifdef PROTOTYPES
void Fun2(struct struct2 foo2)
#else
void Fun2(foo2)
     struct struct2 foo2;
#endif
{
  L2 = foo2;
}
#ifdef PROTOTYPES
void Fun3(struct struct3 foo3)
#else
void Fun3(foo3)
     struct struct3 foo3;
#endif
{
  L3 = foo3;
}
#ifdef PROTOTYPES
void Fun4(struct struct4 foo4)
#else
void Fun4(foo4)
     struct struct4 foo4;
#endif
{
  L4 = foo4;
}
#ifdef PROTOTYPES
void Fun5(struct struct5 foo5)
#else
void Fun5(foo5)
     struct struct5 foo5;
#endif
{
  L5 = foo5;
}
#ifdef PROTOTYPES
void Fun6(struct struct6 foo6)
#else
void Fun6(foo6)
     struct struct6 foo6;
#endif
{
  L6 = foo6;
}
#ifdef PROTOTYPES
void Fun7(struct struct7 foo7)
#else
void Fun7(foo7)
     struct struct7 foo7;
#endif
{
  L7 = foo7;
}
#ifdef PROTOTYPES
void Fun8(struct struct8 foo8)
#else
void Fun8(foo8)
     struct struct8 foo8;
#endif
{
  L8 = foo8;
}
#ifdef PROTOTYPES
void Fun9(struct struct9 foo9)
#else
void Fun9(foo9)
     struct struct9 foo9;
#endif
{
  L9 = foo9;
}
#ifdef PROTOTYPES
void Fun10(struct struct10 foo10)
#else
void Fun10(foo10)
     struct struct10 foo10;
#endif
{
  L10 = foo10; 
}
#ifdef PROTOTYPES
void Fun11(struct struct11 foo11)
#else
void Fun11(foo11)
     struct struct11 foo11;
#endif
{
  L11 = foo11; 
}
#ifdef PROTOTYPES
void Fun12(struct struct12 foo12)
#else
void Fun12(foo12)
     struct struct12 foo12;
#endif
{
  L12 = foo12; 
}
#ifdef PROTOTYPES
void Fun16(struct struct16 foo16)
#else
void Fun16(foo16)
     struct struct16 foo16;
#endif
{
  L16 = foo16; 
}

int main()
{
#ifdef usestubs
  set_debug_traps();
  breakpoint();
#endif

  /* TEST C FUNCTIONS */
  L1  = fun1();	
  L2  = fun2();	
  L3  = fun3();	
  L4  = fun4();	
  L5  = fun5();	
  L6  = fun6();	
  L7  = fun7();	
  L8  = fun8();	
  L9  = fun9();	
  L10 = fun10();
  L11 = fun11();
  L12 = fun12();
  L16 = fun16();

  foo1.a = foo2.a = foo3.a = foo4.a = foo5.a = foo6.a = foo7.a = foo8.a =
    foo9.a = foo10.a = foo11.a = foo12.a = foo16.a = '$';

  Fun1(foo1);	
  Fun2(foo2);	
  Fun3(foo3);	
  Fun4(foo4);	
  Fun5(foo5);	
  Fun6(foo6);	
  Fun7(foo7);	
  Fun8(foo8);	
  Fun9(foo9);	
  Fun10(foo10);
  Fun11(foo11);
  Fun12(foo12);
  Fun16(foo16);

  return 0;
}
