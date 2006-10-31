-- { dg-do compile }

package abstract_limited is
   type I is limited interface;
   type T is abstract limited new I with null record;
end;
