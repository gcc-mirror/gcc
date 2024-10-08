-- { dg-do compile }

package Generic_Inst1 is

   generic
      type Terminals is (<>);
      type Nonterminals is (<>);
   package Types is
      type Action is record
         data : Integer;
      end record;
   end Types;

   generic
      type States is (<>);
      type Input_T is (<>);
      type Action_T is private;
   package FSM is
   end FSM;

   generic
      with package Typs is new Types (<>);
      with package SMs is new FSM
        (States => <>, Input_T => Typs.Terminals, Action_T => Typs.Action);
   package Gen is
   end Gen;

   package Typs is new Types (Natural, Integer);
   package SMs is new FSM (Integer, Natural, Typs.Action);
   package Generator is new Gen (Typs, SMs);

end Generic_Inst1;
