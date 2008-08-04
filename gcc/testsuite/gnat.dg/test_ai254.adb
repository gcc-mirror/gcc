--  { dg-do compile }

procedure test_ai254 is
   function Func
      (Obj : not null access protected function (X : Float) return Float)
      return not null access protected function (X : Float) return Float is
   begin
      return null;
   end;
begin
   null;
end;
