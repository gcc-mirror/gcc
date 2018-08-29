--  { dg-do compile }

procedure Prot4 is
   type App_Priority is (Low, Medium, High);

   function Alpha return App_Priority is
   begin
      return Low;
   end Alpha;

   function Beta return App_Priority is
   begin
      return High;
   end Beta;

   protected Hold is
      entry D7 (App_Priority range Alpha .. Beta);
   end Hold;

   protected body Hold is
      entry D7 (for AP in App_Priority range Alpha .. Beta) when True is
      begin
         null;
      end D7;
   end Hold;
begin
   null;
end;
