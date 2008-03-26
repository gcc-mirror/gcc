package Forward_Anon is
   type Object is null record; 
   function Get_Current return access Object;
   Current_Object : constant access Object;
        
 private
   One_Object : aliased Object;
   Current_Object : constant access Object := One_Object'Access;
end;
