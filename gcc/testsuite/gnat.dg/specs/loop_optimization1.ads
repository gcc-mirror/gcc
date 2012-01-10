-- { dg-do compile }
-- { dg-options "-O3" }

with Loop_Optimization1_Pkg;

package Loop_Optimization1 is

  type Kind_Type is (One, Two, Three, Four);
  type Array_Type is array (Kind_Type) of Boolean;
  pragma Pack (Array_Type);

  package Q is new Loop_Optimization1_Pkg (Boolean, Kind_Type, Array_Type);

end Loop_Optimization1;
