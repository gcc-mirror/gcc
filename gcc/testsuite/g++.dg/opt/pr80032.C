// PR tree-optimization/80032
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }
// If DCE removes too many CLOBBERs then stack usage goes through the
// roof as stack slots can no longer be shared.
// { dg-additional-options "-Wstack-usage=200" { target x86_64-*-* i?86-*-* } }

typedef unsigned a;
namespace test {
    enum b { c };
    class ADataContainer;
    class BitMask;
    namespace api {
	enum DataStore { candidate };
    }
    using d = api::DataStore;
    namespace db {
	class e;
	class f;
	class g;
	class ManagedObjectConst {
	public:
	    ManagedObjectConst(const ManagedObjectConst &);
	    bool isFieldDefault(a, d) const;
	    ADataContainer &getFieldDefault(a, d) const;
	    g *h;
	    e *i;
	    f *j;
	};
	struct FieldInfo {
	    FieldInfo(ManagedObjectConst, a, d);
	    ManagedObjectConst k;
	};
	b compare(const FieldInfo &, const ADataContainer &);
	class ManagedObject : public ManagedObjectConst {};
    }
    using namespace db;
    void FN(ManagedObject &k, const BitMask &) {
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
	if (!k.isFieldDefault(8, d::candidate) &&
	    !compare(FieldInfo(k, 11, d::candidate),
		     k.getFieldDefault(11, d::candidate)) == c)
	  return;
    }
}
