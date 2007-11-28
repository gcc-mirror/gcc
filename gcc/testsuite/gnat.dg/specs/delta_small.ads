-- { dg-do compile }

package Delta_Small is
   type T is delta 0.1 range -0.8 .. 0.8;
   for T'Small use 0.1;
   for T'Size use 4;
   type T2 is new T range -0.4 .. 0.4;
   for T2'Small use 0.0625;
end Delta_Small;
