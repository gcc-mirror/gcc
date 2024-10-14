-- { dg-do compile }

with Generic_Inst2.Child1;

generic
package Generic_Inst2.Child2 is

   package Second is new Generic_Inst2.Child1;

end Generic_Inst2.Child2;
