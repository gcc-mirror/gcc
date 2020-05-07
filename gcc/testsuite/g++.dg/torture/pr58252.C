// { dg-do compile }
// { dg-options "-fpermissive -Wno-return-type" }
typedef long unsigned int size_t;
       typedef bool _CORBA_Boolean;
         typedef unsigned int _CORBA_ULong;
             template <class T> class _CORBA_Sequence {
     public:   typedef _CORBA_Sequence<T> T_seq;
        inline T_seq &operator= (const T_seq &s)   {
         for (unsigned long i=0;
     i < pd_len;
     i++) {
       }
       }
       _CORBA_ULong pd_len;
     };
             template <class T> class _CORBA_Unbounded_Sequence : public _CORBA_Sequence<T> {
        inline _CORBA_Unbounded_Sequence_WChar() { // { dg-warning "forbids declaration" }
       }
     };
       class _CORBA_ObjRef_Var_base {
     };
         template <class T, class T_Helper> class _CORBA_ObjRef_Var : public _CORBA_ObjRef_Var_base {
     public:   typedef T* ptr_t;
       typedef T* T_ptr;
        inline _CORBA_ObjRef_Var() : pd_objref(T_Helper::_nil()) {
    }
       inline _CORBA_ObjRef_Var(T_ptr p) : pd_objref(p) {
       }
      private:   T_ptr pd_objref;
      };
        class omniLocalIdentity;
         class omniObjRef {
     };
            class omniServant {
      public:   virtual ~omniServant();
        virtual void* _ptrToInterface(const char* repoId);
          };
         namespace CORBA  {
      class NVList {
     };
      class Object {
     };
      struct StructMember {
     };
      class StructMemberSeq : public _CORBA_Unbounded_Sequence< StructMember > {
        };
      class _objref_IRObject :   public virtual ::CORBA::Object,   public virtual omniObjRef {
     };
      class _impl_IRObject :   public virtual omniServant {
      };
     class _objref_Container;
      typedef _objref_Container* Container_ptr;
      class _impl_Contained :   public virtual _impl_IRObject {
     };
     class _objref_ExceptionDef;
      typedef _objref_ExceptionDef* ExceptionDef_ptr;
      class ExceptionDef_Helper {
     public:   typedef ExceptionDef_ptr _ptr_type;
        static _ptr_type _nil();
     };
      typedef _CORBA_ObjRef_Var<_objref_ExceptionDef, ExceptionDef_Helper> ExceptionDef_var;
      class Container {
     public:    typedef Container_ptr _ptr_type;
        static const char* _PD_repoId;
       };
      class _objref_Container :   public virtual _objref_IRObject {
       ExceptionDef_ptr create_exception(const char* id, const char* name, const char* version, const ::CORBA::StructMemberSeq& members);
     };
      class _impl_Container :   public virtual _impl_IRObject {
     public:   virtual ~_impl_Container();
       virtual ExceptionDef_ptr create_exception(const char* id, const char* name, const char* version, const ::CORBA::StructMemberSeq& members) = 0;
     };
      class _impl_IDLType :   public virtual _impl_IRObject {
     };
      class _impl_TypedefDef :   public virtual _impl_Contained,   public virtual _impl_IDLType {
     };
      class _impl_StructDef :   public virtual _impl_TypedefDef,   public virtual _impl_Container {
      };
           }
          namespace PortableServer {
            class ServantBase : public virtual omniServant {
    };
             }
         namespace POA_CORBA {
           class IRObject :   public virtual CORBA::_impl_IRObject,   public virtual ::PortableServer::ServantBase {
     };
      class Contained :   public virtual CORBA::_impl_Contained,   public virtual IRObject {
     };
      class Container :   public virtual CORBA::_impl_Container,   public virtual IRObject {
     };
      class IDLType :   public virtual CORBA::_impl_IDLType,   public virtual IRObject {
     };
      class TypedefDef :   public virtual CORBA::_impl_TypedefDef,   public virtual Contained,     public virtual IDLType {
     };
      class StructDef :   public virtual CORBA::_impl_StructDef,   public virtual TypedefDef,     public virtual Container {
     public:   virtual ~StructDef();
     };
       }
         namespace omni {
     class omniOrbPOA;
     class giopAddress;
     }
             class omniCallDescriptor {
     public:   typedef void (*LocalCallFn)(omniCallDescriptor*, omniServant*);
        inline omniCallDescriptor(LocalCallFn lcfn, const char* op_,        int op_len_, _CORBA_Boolean oneway,        const char*const* user_excns_,        int n_user_excns_,                             _CORBA_Boolean is_upcall_)     : pd_localCall(lcfn),       pd_op(op_), pd_oplen(op_len_),       pd_user_excns(user_excns_),       pd_n_user_excns(n_user_excns_),       pd_is_oneway(oneway),       pd_is_upcall(is_upcall_),       pd_contains_values(0),       pd_first_address_used(0),       pd_current_address(0),       pd_objref(0),       pd_poa(0),       pd_localId(0),       pd_deadline_secs(0),       pd_deadline_nanosecs(0) {
    }
      private:   LocalCallFn pd_localCall;
       const char* pd_op;
       size_t pd_oplen;
       const char*const* pd_user_excns;
       int pd_n_user_excns;
       _CORBA_Boolean pd_is_oneway;
       _CORBA_Boolean pd_is_upcall;
       _CORBA_Boolean pd_contains_values;
        const omni::giopAddress* pd_first_address_used;
       const omni::giopAddress* pd_current_address;
           omniObjRef* pd_objref;
        omni::omniOrbPOA* pd_poa;
       omniLocalIdentity* pd_localId;
              unsigned long pd_deadline_secs;
       unsigned long pd_deadline_nanosecs;
      };
          class _0RL_cd_7963219a43724a61_f2000000   : public omniCallDescriptor {
     public:   inline _0RL_cd_7963219a43724a61_f2000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):      omniCallDescriptor(lcfn, op_, oplen, 0, _user_exns, 0, upcall)   {
        }
         static const char* const _user_exns[];
       const char* arg_0;
       const char* arg_1;
       const char* arg_2;
       const CORBA::StructMemberSeq* arg_3;
       CORBA::ExceptionDef_var result;
     };
          static void _0RL_lcfn_7963219a43724a61_03000000(omniCallDescriptor* cd, omniServant* svnt) {
       _0RL_cd_7963219a43724a61_f2000000* tcd = (_0RL_cd_7963219a43724a61_f2000000*)cd;
       CORBA::_impl_Container* impl = (CORBA::_impl_Container*) svnt->_ptrToInterface(CORBA::Container::_PD_repoId);
       tcd->result = impl->create_exception(tcd->arg_0, tcd->arg_1, tcd->arg_2, *tcd->arg_3);
       }
         CORBA::ExceptionDef_ptr CORBA::_objref_Container::create_exception(const char* id, const char* name, const char* version, const ::CORBA::StructMemberSeq& members) {
       _0RL_cd_7963219a43724a61_f2000000 _call_desc(_0RL_lcfn_7963219a43724a61_03000000, "create_exception", 17);
       }
         POA_CORBA::StructDef::~StructDef() {
    }
