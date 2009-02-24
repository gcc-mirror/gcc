/* PR c++/39242, xplicit instantiation declaration prohibits implicit
   instantiation of non-inline functions.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

class Rep {
public:
    void unref() const { }
    static void unref (const Rep * obj_r) { obj_r->unref(); }
};
template<typename _Tp, typename _Bt = _Tp>
class RepPtrStore {
    _Tp * _obj;
    void _assign( _Tp * new_r );
public:
    ~RepPtrStore() { _assign( 0 ); }
};
template<typename _Tp,typename _Bt>
void RepPtrStore<_Tp,_Bt>::_assign( _Tp * new_r )
{
  Rep::unref( _obj );
}
class RepPtrBase { };
template<typename _Bt> class PtrBase : public RepPtrBase { };
template<typename _Tp, typename _Bt = _Tp>
class Ptr : public PtrBase<_Bt> {
    RepPtrStore<_Tp,_Bt> _ptr;
};
class YCode;
class YStatement;
typedef Ptr<YStatement,YCode> YStatementPtr;
extern template class RepPtrStore<YStatement,YCode>;
class ExecutionEnvironment {
    YStatementPtr m_statement;
    ~ExecutionEnvironment() { };
};

