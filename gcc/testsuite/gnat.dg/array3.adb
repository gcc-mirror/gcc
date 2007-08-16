--  { dg-do run }

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
procedure array3 is
    type Method_Kinds is (Signal, Slot, Method);

    package Unbounded_String_Vectors is
      new Ada.Containers.Vectors
           (Positive, Ada.Strings.Unbounded.Unbounded_String);

    Params_Vector : Unbounded_String_Vectors.Vector;

    type Method_Info is record
       Name        : Ada.Strings.Unbounded.Unbounded_String;
       Signature   : Ada.Strings.Unbounded.Unbounded_String;
       Parameters  : Unbounded_String_Vectors.Vector;
       Kind        : Method_Kinds;
    end record;

    package Method_Info_Vectors is
      new Ada.Containers.Vectors (Positive, Method_Info);

    Signals : Method_Info_Vectors.Vector;
begin
    
    Unbounded_String_Vectors.Append
      (Params_Vector,
       Ada.Strings.Unbounded.To_Unbounded_String ("AAA"));

    Method_Info_Vectors.Append
      (Signals,
       (Name        => To_Unbounded_String (""),
        Signature   => To_Unbounded_String (""),
        Parameters  => Params_Vector,
        Kind        => Signal));
end;
