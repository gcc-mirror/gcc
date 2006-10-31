package ICE_Types is
   type Float_View_T is private;
   procedure Initialize (X : out Float_View_T);
private
   type Float_View_T is new Float;
end;
