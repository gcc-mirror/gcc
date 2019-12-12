--  { dg-do compile }

procedure Annotation1 is
   pragma Annotate (Some_Tool, Some_Action, "abc" & "def");
begin
   null;
end Annotation1;