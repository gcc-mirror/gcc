--  { dg-do compile }

with Dimensions2_phys; use Dimensions2_phys;

procedure Dimensions2 is

   zero_flow : constant Volumetric_Flow := 0.0 * m**3 / h;
   type Node_Flow_Scenario_T is array (Positive range <>)
          of Volumetric_Flow with
            default_component_value => zero_flow;
   subtype Max_Node_Flow_Scenario_T
       is Node_Flow_Scenario_T (Natural (1) .. 48);
   flow_value_array : Max_Node_Flow_Scenario_T  := (1..48 => zero_flow);
   flow_value_array1 : Max_Node_Flow_Scenario_T
        := (Max_Node_Flow_Scenario_T'Range=> zero_flow);
   flow_value_array2 : Max_Node_Flow_Scenario_T  := (others => zero_flow);

begin
    null;
end Dimensions2;
