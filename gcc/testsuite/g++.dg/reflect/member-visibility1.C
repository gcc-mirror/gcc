// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Tests std::meta::is_public, std::meta::is_private, std::meta::is_protected
#include <meta>

class PublicBase {  };
class ProtectedBase {  };
class PrivateBase {  };
class VirtualBase {  };
class VirtualProtectedBase {  };

class A : public PublicBase, protected ProtectedBase, private PrivateBase, 
	  virtual VirtualBase, protected virtual VirtualProtectedBase
{
  void test_bases_visibility()
  {
    constexpr auto ctx = std::meta::access_context::unchecked();
    static_assert (std::meta::bases_of (^^A, ctx).size() == 5);

    static_assert (std::meta::is_public (std::meta::bases_of (^^A, ctx)[0]));
    static_assert (std::meta::is_protected (std::meta::bases_of (^^A, ctx)[1]));
    static_assert (std::meta::is_private (std::meta::bases_of (^^A, ctx)[2]));
    static_assert (std::meta::is_private (std::meta::bases_of (^^A, ctx)[3]));
    static_assert (std::meta::is_protected (std::meta::bases_of (^^A, ctx)[4]));
  }

// private:
  A(int)
  {
    int a;
    static_assert (!std::meta::is_public (std::meta::parent_of (^^a)));
    static_assert (!std::meta::is_protected (std::meta::parent_of (^^a)));
    static_assert (std::meta::is_private (std::meta::parent_of (^^a)));
  }

protected:
  A(bool)
  {
    int a;
    static_assert (!std::meta::is_public (std::meta::parent_of (^^a)));
    static_assert (std::meta::is_protected (std::meta::parent_of (^^a)));
    static_assert (!std::meta::is_private (std::meta::parent_of (^^a)));
  }

  ~A() = default;

public:
  A()
  {
    int a;
    static_assert (std::meta::is_public (std::meta::parent_of (^^a)));
    static_assert (!std::meta::is_protected (std::meta::parent_of (^^a)));
    static_assert (!std::meta::is_private (std::meta::parent_of (^^a)));
  }

  int public_field;
  static int public_static_field;

  int public_function();
  static int public_static_function();
  virtual void public_virtual_function() = 0;

  template <typename T> 
  void public_template_function();
  template <typename T> 
  static void public_static_template_function();

  using public_type_alias = int;

  struct PublicCls {};

  enum PublicEnum {
    B, C, D
  };

  enum class PublicEnumClass {
    E, F
  };

  union PublicUnion {};

protected:

  int protected_field;
  static int protected_static_field;

  int protected_function();
  static int protected_static_function();
  virtual void protected_virtual_function() = 0;

  template <typename T> 
  void protected_template_function();
  template <typename T> 
  static void protected_static_template_function();

  using protected_type_alias = int;

  struct ProtectedCls {};

  enum ProtectedEnum {
    G, H
  };

  enum class ProtectedEnumClass {
    I, J
  };

  union ProtectedUnion {};

private:

  int private_field;
  static int private_static_field;

  int private_function();
  static int private_static_function();
  virtual void private_virtual_function() = 0;

  template <typename T> 
  void private_template_function();
  template <typename T> 
  static void private_static_template_function();

  using private_type_alias = int;

  struct PrivateCls {};

  enum PrivateEnum {
    K, L
  };

  enum class PrivateEnumClass {
    M, N
  };

  union PrivateUnion {};


  void test_is_public()
  {
    // positive cases for public class members
    static_assert (std::meta::is_public (^^public_field));
    static_assert (std::meta::is_public (^^public_static_field));

    static_assert (std::meta::is_public(^^public_function));
    static_assert (std::meta::is_public (^^public_static_function));
    static_assert (std::meta::is_public (^^public_virtual_function));

    static_assert (std::meta::is_public (^^public_template_function));
    static_assert (std::meta::is_public (^^public_template_function<int>));

    static_assert (std::meta::is_public (^^public_static_template_function));
    static_assert (std::meta::is_public (^^public_static_template_function<int>));

    static_assert (std::meta::is_public (^^public_type_alias));
    static_assert (std::meta::is_public (^^PublicCls));
    static_assert (std::meta::is_public (^^PublicUnion));

    static_assert (std::meta::is_public (^^PublicEnum));
    static_assert (std::meta::is_public (^^PublicEnumClass)); 

    static_assert (std::meta::is_public (^^B));
    static_assert (std::meta::is_public (^^C));
    static_assert (std::meta::is_public (^^D));

    // negative test cases
    static_assert (!std::meta::is_public (^^A));

    // scoped enum values are not class members
    static_assert (!std::meta::is_public (^^PublicEnumClass::E));
    static_assert (!std::meta::is_public (^^PublicEnumClass::F));

    static_assert (!std::meta::is_public (^^ProtectedEnumClass::I));
    static_assert (!std::meta::is_public (^^ProtectedEnumClass::J));

    static_assert (!std::meta::is_public (^^PrivateEnumClass::M));
    static_assert (!std::meta::is_public (^^PrivateEnumClass::N));

    // protected class members
    static_assert (!std::meta::is_public (^^~A));

    static_assert (!std::meta::is_public (^^protected_field));
    static_assert (!std::meta::is_public (^^protected_static_field));

    static_assert (!std::meta::is_public (^^protected_function));
    static_assert (!std::meta::is_public (^^protected_static_function));
    static_assert (!std::meta::is_public (^^protected_virtual_function));

    static_assert (!std::meta::is_public (^^protected_template_function));
    static_assert (!std::meta::is_public (^^protected_template_function<int>));

    static_assert (!std::meta::is_public (^^protected_static_template_function));
    static_assert (!std::meta::is_public (^^protected_static_template_function<int>));

    static_assert (!std::meta::is_public (^^protected_type_alias)); 
    static_assert (!std::meta::is_public (^^ProtectedCls));  
    static_assert (!std::meta::is_public (^^ProtectedUnion));

    static_assert (!std::meta::is_public (^^ProtectedEnum));
    static_assert (!std::meta::is_public (^^ProtectedEnumClass)); 

    static_assert (!std::meta::is_public (^^G));
    static_assert (!std::meta::is_public (^^H));

    // private class members
    static_assert (!std::meta::is_public (^^private_field));
    static_assert (!std::meta::is_public (^^private_static_field));

    static_assert (!std::meta::is_public (^^private_function));
    static_assert (!std::meta::is_public (^^private_static_function));
    static_assert (!std::meta::is_public (^^private_virtual_function));

    static_assert (!std::meta::is_public (^^private_template_function));
    static_assert (!std::meta::is_public (^^private_template_function<int>));

    static_assert (!std::meta::is_public (^^private_static_template_function));
    static_assert (!std::meta::is_public (^^private_static_template_function<int>));

    static_assert (!std::meta::is_public (^^private_type_alias)); 
    static_assert (!std::meta::is_public (^^PrivateCls));  
    static_assert (!std::meta::is_public (^^PrivateUnion));

    static_assert (!std::meta::is_public (^^PrivateEnum));
    static_assert (!std::meta::is_public (^^PrivateEnumClass)); 

    static_assert (!std::meta::is_public (^^K));
    static_assert (!std::meta::is_public (^^L));
  }

  void test_is_protected()
  {
    // positive cases for protected class members
    static_assert (std::meta::is_protected (^^~A));

    static_assert (std::meta::is_protected (^^protected_field));
    static_assert (std::meta::is_protected (^^protected_static_field));

    static_assert (std::meta::is_protected (^^protected_function));
    static_assert (std::meta::is_protected (^^protected_static_function));
    static_assert (std::meta::is_protected (^^protected_virtual_function));

    static_assert (std::meta::is_protected (^^protected_template_function));
    static_assert (std::meta::is_protected (^^protected_template_function<int>));

    static_assert (std::meta::is_protected (^^protected_static_template_function));
    static_assert (std::meta::is_protected (^^protected_static_template_function<int>));

    static_assert (std::meta::is_protected (^^protected_type_alias)); 
    static_assert (std::meta::is_protected (^^ProtectedCls));  
    static_assert (std::meta::is_protected (^^ProtectedUnion));

    static_assert (std::meta::is_protected (^^ProtectedEnum));
    static_assert (std::meta::is_protected (^^ProtectedEnumClass)); 

    static_assert (std::meta::is_protected (^^G));
    static_assert (std::meta::is_protected (^^H));

    // negative cases:
    static_assert (!std::meta::is_protected (^^A));
    // scoped enum values are not class members
    static_assert (!std::meta::is_protected (^^PublicEnumClass::E));
    static_assert (!std::meta::is_protected (^^PublicEnumClass::F));

    static_assert (!std::meta::is_protected (^^ProtectedEnumClass::I));
    static_assert (!std::meta::is_protected (^^ProtectedEnumClass::J));

    static_assert (!std::meta::is_protected (^^PrivateEnumClass::M));
    static_assert (!std::meta::is_protected (^^PrivateEnumClass::N));

    // public class members
    static_assert (!std::meta::is_protected (^^public_field));
    static_assert (!std::meta::is_protected (^^public_static_field));

    static_assert (!std::meta::is_protected (^^public_function));
    static_assert (!std::meta::is_protected (^^public_static_function));
    static_assert (!std::meta::is_protected (^^public_virtual_function));

    static_assert (!std::meta::is_protected (^^public_template_function));
    static_assert (!std::meta::is_protected (^^public_template_function<int>));

    static_assert (!std::meta::is_protected (^^public_static_template_function));
    static_assert (!std::meta::is_protected (^^public_static_template_function<int>));

    static_assert (!std::meta::is_protected (^^public_type_alias)); 
    static_assert (!std::meta::is_protected (^^PublicCls));  
    static_assert (!std::meta::is_protected (^^PublicUnion));

    static_assert (!std::meta::is_protected (^^PublicEnum));
    static_assert (!std::meta::is_protected (^^PublicEnumClass)); 

    static_assert (!std::meta::is_protected (^^B));
    static_assert (!std::meta::is_protected (^^C));
    static_assert (!std::meta::is_protected (^^D));


    // private class members
    static_assert (!std::meta::is_protected (^^private_field));
    static_assert (!std::meta::is_protected (^^private_static_field));

    static_assert (!std::meta::is_protected (^^private_function));
    static_assert (!std::meta::is_protected (^^private_static_function));
    static_assert (!std::meta::is_protected (^^private_virtual_function));

    static_assert (!std::meta::is_protected (^^private_template_function));
    static_assert (!std::meta::is_protected (^^private_template_function<int>));

    static_assert (!std::meta::is_protected (^^private_static_template_function));
    static_assert (!std::meta::is_protected (^^private_static_template_function<int>));

    static_assert (!std::meta::is_protected (^^private_type_alias)); 
    static_assert (!std::meta::is_protected (^^PrivateCls));  
    static_assert (!std::meta::is_protected (^^PrivateUnion));

    static_assert (!std::meta::is_protected (^^PrivateEnum));
    static_assert (!std::meta::is_protected (^^PrivateEnumClass)); 

    static_assert (!std::meta::is_protected (^^K));
    static_assert (!std::meta::is_protected (^^L));
  }

  void test_is_private()
  {
    // positive test cases: private class members
    static_assert (std::meta::is_private (^^private_field));
    static_assert (std::meta::is_private (^^private_static_field));

    static_assert (std::meta::is_private (^^private_function));
    static_assert (std::meta::is_private (^^private_static_function));
    static_assert (std::meta::is_private (^^private_virtual_function));

    static_assert (std::meta::is_private (^^private_template_function));
    static_assert (std::meta::is_private (^^private_template_function<int>));

    static_assert (std::meta::is_private (^^private_static_template_function));
    static_assert (std::meta::is_private (^^private_static_template_function<int>));

    static_assert (std::meta::is_private (^^private_type_alias)); 
    static_assert (std::meta::is_private (^^PrivateCls));  
    static_assert (std::meta::is_private (^^PrivateUnion));

    static_assert (std::meta::is_private (^^PrivateEnum));
    static_assert (std::meta::is_private (^^PrivateEnumClass)); 

    static_assert (std::meta::is_private (^^K));
    static_assert (std::meta::is_private (^^L));

    // negative cases:
    static_assert (!std::meta::is_private (^^A));

    // scoped enum values are not class members
    static_assert (!std::meta::is_private (^^PublicEnumClass::E));
    static_assert (!std::meta::is_private (^^PublicEnumClass::F));

    static_assert (!std::meta::is_private (^^ProtectedEnumClass::I));
    static_assert (!std::meta::is_private (^^ProtectedEnumClass::J));

    static_assert (!std::meta::is_private (^^PrivateEnumClass::M));
    static_assert (!std::meta::is_private (^^PrivateEnumClass::N));

    // public class members
    static_assert (!std::meta::is_private (^^public_field));
    static_assert (!std::meta::is_private (^^public_static_field));

    static_assert (!std::meta::is_private (^^public_function));
    static_assert (!std::meta::is_private (^^public_static_function));
    static_assert (!std::meta::is_private (^^public_virtual_function));

    static_assert (!std::meta::is_private (^^public_template_function));
    static_assert (!std::meta::is_private (^^public_template_function<int>));

    static_assert (!std::meta::is_private (^^public_static_template_function));
    static_assert (!std::meta::is_private (^^public_static_template_function<int>));

    static_assert (!std::meta::is_private (^^public_type_alias)); 
    static_assert (!std::meta::is_private (^^PublicCls));  
    static_assert (!std::meta::is_private (^^PublicUnion));

    static_assert (!std::meta::is_private (^^PublicEnum));
    static_assert (!std::meta::is_private (^^PublicEnumClass)); 

    static_assert (!std::meta::is_private (^^B));
    static_assert (!std::meta::is_private (^^C));
    static_assert (!std::meta::is_private (^^D));

    // protected class members
    static_assert (!std::meta::is_private (^^~A));

    static_assert (!std::meta::is_private (^^protected_field));
    static_assert (!std::meta::is_private (^^protected_static_field));

    static_assert (!std::meta::is_private (^^protected_function));
    static_assert (!std::meta::is_private (^^protected_static_function));
    static_assert (!std::meta::is_private (^^protected_virtual_function));

    static_assert (!std::meta::is_private (^^protected_template_function));
    static_assert (!std::meta::is_private (^^protected_template_function<int>));

    static_assert (!std::meta::is_private (^^protected_static_template_function));
    static_assert (!std::meta::is_private (^^protected_static_template_function<int>));

    static_assert (!std::meta::is_private (^^protected_type_alias)); 
    static_assert (!std::meta::is_private (^^ProtectedCls));  
    static_assert (!std::meta::is_private (^^ProtectedUnion));

    static_assert (!std::meta::is_private (^^ProtectedEnum));
    static_assert (!std::meta::is_private (^^ProtectedEnumClass)); 

    static_assert (!std::meta::is_private (^^G));
    static_assert (!std::meta::is_private (^^H));
  }
};

