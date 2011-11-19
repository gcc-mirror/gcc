with Loop_Optimization10_Pkg; use Loop_Optimization10_Pkg;
package Loop_Optimization10 is

   type Dual_Axis_Type is (One, Two);

   type Array_Real_Type    is array (Dual_Axis_Type) of Float;
   type Array_Limit_Type   is array (Dual_Axis_Type) of Limit_Type;

   function F (Low, High : in Array_Real_Type) return Array_Limit_Type;

end Loop_Optimization10;
