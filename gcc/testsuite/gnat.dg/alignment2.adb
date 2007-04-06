-- { dg-do run }

procedure alignment2 is
   
   pragma COMPONENT_ALIGNMENT(STORAGE_UNIT);
   
   MAX_LIST_SIZE : constant INTEGER := 128*16;
   
   LEVEL2_SIZE : constant INTEGER := 128;
           
           LEVEL1_SIZE : constant INTEGER
              := (MAX_LIST_SIZE - 1) / LEVEL2_SIZE + 1;
           
           type LEVEL2_ARRAY_TYPE is
              array (1..LEVEL2_SIZE) of Integer;
           
           type LEVEL2_TYPE is
              record
                 NUM  : INTEGER := 0;
                 DATA : LEVEL2_ARRAY_TYPE := ( others => 0 );
              end record;
           
           type LEVEL2_PTR_TYPE is access all LEVEL2_TYPE;
           
           type LEVEL1_ARRAY_TYPE is
              array( 1..LEVEL1_SIZE ) of LEVEL2_PTR_TYPE;
           
           type LEVEL1_TYPE is
              record
                 LAST_LINE  : INTEGER := 0;
                 LEVEL2_PTR : LEVEL1_ARRAY_TYPE;
              end record;
           
           L1 : LEVEL1_TYPE;
           L2 : aliased LEVEL2_TYPE;
   
   procedure q (LA : in out LEVEL1_ARRAY_TYPE) is
   begin
      LA (1) := L2'Access;
   end;

begin
   q (L1.LEVEL2_PTR);
   if L1.LEVEL2_PTR (1) /= L2'Access then
     raise Program_Error;
   end if;
end;
