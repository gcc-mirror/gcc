package Assert2
    with SPARK_Mode
is
   type Living is new Integer;
   function Is_Martian (Unused : Living) return Boolean is (False);

   function Is_Green (Unused : Living) return Boolean is (True);

   pragma Assert
     (for all M in Living => (if Is_Martian (M) then Is_Green (M)));
   pragma Assert
     (for all M in Living => (if Is_Martian (M) then not Is_Green (M)));

   procedure Dummy;
end Assert2;
