// PR tree-optimization/56029
// { dg-do compile }

template <class T>
struct DefaultDeleter
{
  void operator () (T * ptr) { delete ptr; }
};
template <class T, class D>
struct scoped_ptr_impl
{
  scoped_ptr_impl (T * p):data_ (p) {}
  template <typename U, typename V>
  scoped_ptr_impl (scoped_ptr_impl <U, V> *other):data_ (other->release (), get_deleter ()) {}
  ~scoped_ptr_impl () { static_cast <D> (data_) (data_.ptr); }
  void reset (T * p) { data_.ptr = p; }
  D get_deleter () {}
  T *release () { data_.ptr = __null; }
  struct Data
  : D
  {
    Data (T *) : ptr () {}
    Data (T *, D) : D (), ptr () {}
    T *ptr;
  };
  Data data_;
};
template <class T, class D = DefaultDeleter <T> >
struct scoped_ptr
{
  struct RValue
  {
    RValue (scoped_ptr * object):object (object) {}
    scoped_ptr *object;
  };
  scoped_ptr Pass () { return scoped_ptr ((this)); }
  typedef T element_type;
  typedef D deleter_type;
  scoped_ptr () : impl_ (__null) {}
  scoped_ptr (RValue rvalue) : impl_ (&rvalue.object->impl_) {}
  void reset (element_type * p) { impl_.reset (p); }
  scoped_ptr_impl <element_type, deleter_type> impl_;
};
template <typename>
struct Callback;
struct ClientSocketFactory;
struct DatagramClientSocket;
struct DnsSocketPool
{
  scoped_ptr <DatagramClientSocket> CreateConnectedSocket ();
  ClientSocketFactory *socket_factory_;
};
int RandInt (int, int);
struct BindStateBase {};
struct CallbackBase
{
  CallbackBase (BindStateBase *);
  ~CallbackBase ();
};
template <typename, typename, typename>
struct BindState;
template <typename R, typename A1, typename A2>
struct Callback <R (A1, A2)> : CallbackBase
{
  template <typename Runnable, typename BindRunType, typename BoundArgsType>
  Callback (BindState <Runnable, BindRunType, BoundArgsType> *bind_state) : CallbackBase (bind_state) {}
};
typedef Callback <int (int, int)>
RandIntCallback;
struct ClientSocketFactory
{
  virtual DatagramClientSocket *CreateDatagramClientSocket (RandIntCallback) = 0;
};
template <typename>
struct RunnableAdapter;
template <typename R, typename A1, typename A2>
struct RunnableAdapter <R (*) (A1, A2)>
{
  typedef R (RunType) (A1, A2);
};
template <typename T>
struct FunctorTraits
{
  typedef RunnableAdapter <T> RunnableType;
  typedef typename RunnableType::RunType RunType;
};
template <typename T>
typename FunctorTraits <T>::RunnableType MakeRunnable (T)
{
}
template <int, typename, typename>
struct Invoker;
template <typename StorageType, typename R, typename X1, typename X2>
struct Invoker <0, StorageType, R (X1, X2)>
{
  typedef R (UnboundRunType) (X1, X2);
};
template <typename Runnable, typename RunType>
struct BindState <Runnable, RunType, void ()> : BindStateBase
{
  typedef Runnable RunnableType;
  typedef Invoker <0, BindState, RunType> InvokerType;
  typedef typename InvokerType::UnboundRunType UnboundRunType;
  BindState (Runnable):runnable_ () {}
  RunnableType runnable_;
};
template <typename Functor>
Callback <typename BindState <typename FunctorTraits <Functor>::RunnableType, typename FunctorTraits <Functor>::RunType, void ()>::UnboundRunType>
Bind (Functor functor)
{
  typedef typename FunctorTraits <Functor>::RunnableType RunnableType;
  typedef typename FunctorTraits <Functor>::RunType RunType;
  typedef BindState <RunnableType, RunType, void ()> BindState;
  Callback <typename BindState::UnboundRunType> (new BindState (MakeRunnable (functor)));
}
struct DatagramClientSocket
{
  virtual ~ DatagramClientSocket ();
};
scoped_ptr <DatagramClientSocket>
DnsSocketPool::CreateConnectedSocket ()
{
  scoped_ptr <DatagramClientSocket> socket;
  socket.reset (socket_factory_->CreateDatagramClientSocket (Bind (RandInt)));
  socket.Pass ();
}
