-- { dg-do compile }

generic

  type Ext is new T with private;

package Private3.Child is

  procedure P_Private (X : in out Ext) is null;

end Private3.Child;
