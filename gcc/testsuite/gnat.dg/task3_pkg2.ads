with Task3_Pkg1; use Task3_Pkg1;

package Task3_Pkg2 is
   type Root (Discr : Integer) is tagged limited record
      Wrap : Task_Wrapper (Discr);
   end record;
end Task3_Pkg2;
