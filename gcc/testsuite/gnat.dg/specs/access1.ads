--  { dg-do compile }

package Access1 is

   type R;
   type S is access R;
   type R is new S;

end Access1;
