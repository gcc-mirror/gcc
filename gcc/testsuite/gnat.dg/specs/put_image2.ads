-- { dg-do compile }
-- { dg-options "-gnat2022" }

package Put_Image2 is

  type Abstract_Configuration_Provider is tagged;

  type Configuration_Provider_Access is
    access all Abstract_Configuration_Provider'Class;

  type Abstract_Configuration_Provider
    (Child : Configuration_Provider_Access := null) is
    abstract tagged limited private;

private

  type Abstract_Configuration_Provider
    (Child : Configuration_Provider_Access := null) is
    abstract tagged limited null record;

end Put_Image2;
