-- { dg-do compile } 

package double_record_extension1 is

   type T1(n: natural) is tagged record
      s1: string (1..n);
   end record;
   type T2(j,k: natural) is new T1(j) with record
      s2: string (1..k);
   end record;
   type T3 is new T2 (10, 10) with null record;

end double_record_extension1;
