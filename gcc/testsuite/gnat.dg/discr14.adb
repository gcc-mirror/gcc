-- { dg-do compile }

package body Discr14 is

   procedure ASSIGN( TARGET : in out SW_TYPE_INFO ;
                     SOURCE : in     SW_TYPE_INFO ) is
   begin
      TARGET := new T_SW_TYPE_DESCRIPTOR( SOURCE.SW_TYPE, SOURCE.DIMENSION );
   end ASSIGN;

end Discr14;
