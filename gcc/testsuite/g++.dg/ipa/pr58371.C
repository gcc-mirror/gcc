/* { dg-do compile } */
/* { dg-options "-O2" } */


typedef int size_t;
namespace {
template < typename > struct char_traits;
}
namespace __gnu_cxx {
template < typename > class new_allocator {
};
}
namespace std {
template < typename _Tp > class allocator:__gnu_cxx::new_allocator < _Tp > {
public:
  size_t size_type;
  typedef _Tp & const_reference;
  template < typename > struct rebind {
    typedef allocator other;
  };
};
}
namespace __gnu_cxx {
template < typename _Alloc > struct __alloc_traits {
  typedef typename _Alloc::const_reference const_reference;
  template < typename _Tp > struct rebind {
    typedef typename _Alloc::template rebind < _Tp >::other other;
  };
};
}
namespace std {
struct __numeric_limits_base {
};
template < typename _Tp > struct numeric_limits:__numeric_limits_base {
  static _Tp max () {
  }
};
template < typename _Tp, typename _Alloc > struct _Vector_base {
  typedef typename __gnu_cxx::__alloc_traits < _Alloc >::template rebind <
  _Tp >::other _Tp_alloc_type;
};
template < typename _Tp, typename _Alloc = std::allocator < _Tp > >class vector:_Vector_base < _Tp,
  _Alloc
    > {
  typedef _Vector_base < _Tp, _Alloc > _Base;
  typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
  typedef __gnu_cxx::__alloc_traits < _Tp_alloc_type > _Alloc_traits;
public:
  _Tp value_type;
  typedef typename _Alloc_traits::const_reference const_reference;
  typedef size_t size_type;
  size_type size () {
  } const_reference operator[] (size_type) {
  }
};
template < typename _CharT, typename =
char_traits < _CharT > >class basic_ostream;
typedef basic_ostream < int >ostream;
class ios_base {
};
template < typename, typename > class basic_ios:ios_base {
};
template < typename _CharT, typename _Traits > class basic_ostream:basic_ios < _CharT,
  _Traits
    > {
public:
  _CharT char_type;
  typedef basic_ostream __ostream_type;
  __ostream_type & operator<< (const void *) {
  }
};
}
namespace logging {
int GetMinLogLevel ();
typedef int LogSeverity;
LogSeverity LOG_ERROR_REPORT;
LogSeverity LOG_DCHECK;
class LogMessage {
public:
  LogMessage (const char *, int, LogSeverity);
  std::ostream & stream () {
  }
};
class LogMessageVoidify {
public:
  LogMessageVoidify () {
  } void operator& (std::ostream &) {
  }
};
}
namespace base {
namespace internal {
class WeakPtrBase {
};
class SupportsWeakPtrBase {
};
} template < typename T > class WeakPtr:internal::WeakPtrBase {
public:
  WeakPtr () :ptr_ () {
  } T *operator-> () {
    logging:0 &&
    logging::LOG_DCHECK >=
    logging::GetMinLogLevel () ? (void) 0 : logging::
    LogMessageVoidify () & logging::
    LogMessage ("../../base/memory/weak_ptr.h", 0,
                logging::LOG_ERROR_REPORT).stream () << ". ";
  } T *ptr_;
};
template < class > class SupportsWeakPtr:internal::SupportsWeakPtrBase {
};
}
template < class ObserverType > class ObserverListBase:base::SupportsWeakPtr < ObserverListBase < ObserverType >
    > {
public:
  class Iterator {
  public:
    Iterator (ObserverListBase & list) :max_index_ (0 ? std::numeric_limits <
          size_t >::max () : list.observers_.
          size () ) {
    } ObserverType *
    GetNext () {
      ListType & observers = list_->observers_;
      if (observers[0])
        ++index_;
    }
    base::WeakPtr < ObserverListBase > list_;
    size_t
    index_;
    size_t
    max_index_;
  };
  typedef
  std::vector <
  ObserverType * >
  ListType;
  ListType
  observers_;
};
template < class ObserverType, bool > class ObserverList:public ObserverListBase <
    ObserverType > {
};
namespace
    ProxyPrefs {
enum ConfigState
{ };
}
namespace
    net {
class
    ProxyConfig {
};
class
    ProxyConfigService {
public:
  enum ConfigAvailability
  { };
  class
      Observer {
  public:
    Observer () {
    } virtual void
    OnProxyConfigChanged (const ProxyConfig &, ConfigAvailability) = 0;
  };
  virtual void
  OnLazyPoll () {
  }
};
}
class
  ChromeProxyConfigService:
  net::ProxyConfigService,
    net::ProxyConfigService::Observer {
  ConfigAvailability
  GetLatestProxyConfig (net::ProxyConfig *);
  void
  UpdateProxyConfig (ProxyPrefs::ConfigState, const net::ProxyConfig &);
  void
  OnProxyConfigChanged (const net::ProxyConfig &, ConfigAvailability);
  ObserverList <
  net::ProxyConfigService::Observer,
      0 >
      observers_;
};
void
ChromeProxyConfigService::UpdateProxyConfig (ProxyPrefs::ConfigState,
    const net::ProxyConfig &) {
  net::ProxyConfig new_config;
  ConfigAvailability availability = GetLatestProxyConfig (0);
net:
  ProxyConfigService::Observer * obs;
  obs->OnProxyConfigChanged (new_config, availability);
}
void
ChromeProxyConfigService::OnProxyConfigChanged (const net::ProxyConfig &,
    ConfigAvailability
    availability) {
  net::ProxyConfig actual_config;
  ObserverListBase <
  net::ProxyConfigService::Observer >::Iterator it (observers_);
net:
  ProxyConfigService::Observer * obs;
  if (it.GetNext () )
    obs->OnProxyConfigChanged (actual_config, availability);
}
