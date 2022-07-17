// PR middle-end/104492
// { dg-do compile { target c++14 } }
// { dg-options "-O3 -Wall" }

namespace std {
typedef decltype (sizeof 0) size_t;
template <typename _Tp, _Tp __v> struct integral_constant {
  static constexpr _Tp value = __v;
};
template <typename _Tp, typename _Up>
struct is_same : integral_constant<bool, __is_same(_Tp, _Up)> {};
template <bool, typename> struct enable_if;
template <typename _Tp> _Tp forward;
}
class QString;
struct random_access_iterator_tag {};
template <typename> struct iterator_traits {
  typedef random_access_iterator_tag iterator_category;
};
template <typename _Iter>
typename iterator_traits<_Iter>::iterator_category __iterator_category(_Iter);
namespace __gnu_cxx {
namespace __ops {
template <typename> struct _Iter_equals_val {
  template <typename _Iterator> bool operator()(_Iterator);
};
template <typename _Value> _Iter_equals_val<_Value> __iter_equals_val(_Value);
}
}
namespace std {
template <typename _RandomAccessIterator, typename _Predicate>
_RandomAccessIterator __find_if(_RandomAccessIterator __first,
				_RandomAccessIterator __last, _Predicate __pred,
				random_access_iterator_tag) {
  if (__pred(__first))
    return __first;
  return __last;
}
template <typename _Iterator, typename _Predicate>
_Iterator __find_if(_Iterator __first, _Iterator __last, _Predicate __pred) {
  return __find_if(__first, __last, __pred, __iterator_category(__first));
}
template <typename _Tp, size_t _Nm> _Tp *begin(_Tp (&__arr)[_Nm]) {
  return __arr;
}
template <typename _Tp, size_t _Nm> _Tp *end(_Tp (&__arr)[_Nm]) {
  return __arr + _Nm;
}
template <typename _InputIterator, typename _Tp>
_InputIterator find(_InputIterator __first, _InputIterator __last, _Tp __val) {
  return __find_if(__first, __last, __gnu_cxx::__ops::__iter_equals_val(__val));
}
}
struct QStringView {
  template <typename T>
  using if_compatible_qstring_like = std::enable_if<std::is_same<T, QString>::value, bool>;
  template <typename String> QStringView(String);
};
template <typename Haystack, typename> struct QStringTokenizerBase {
  class sentinel {};
  struct iterator {
    Haystack operator*();
    iterator operator++(int);
    bool operator!=(sentinel);
  };
  iterator begin();
  template <bool = std::is_same<iterator, sentinel>::value> sentinel end();
};
namespace QtPrivate {
namespace Tok {
template <typename Haystack, typename Needle>
using TokenizerBase = QStringTokenizerBase<Haystack, Needle>;
}
}
template <typename Haystack, typename Needle>
struct QStringTokenizer
    : public QtPrivate::Tok::TokenizerBase<Haystack, Needle> {
  QStringTokenizer(Haystack, Needle);
};
namespace QtPrivate {
namespace Tok {
template <typename Haystack, typename Needle>
using TokenizerResult = QStringTokenizer<Haystack, Needle>;
}
}
template <typename Haystack, typename Needle>
auto qTokenize(Haystack, Needle)
    -> decltype(QtPrivate::Tok::TokenizerResult<Haystack, Needle>{
	std::forward<Haystack>, std::forward<Needle>});
struct QLatin1String {
  QLatin1String(const char *) {}
};
class QString {};
class QLibrary {
  bool isLibrary(const QString &);
};

bool QLibrary::isLibrary(const QString &fileName)
{
    QString completeSuffix = fileName;
    auto isValidSuffix = [](QStringView s) {
	const QLatin1String candidates[] = {
	    QLatin1String("so"),
	};
	return std::find(std::begin(candidates), std::end(candidates), s) != std::end(candidates);
    };
    auto suffixes = qTokenize(completeSuffix, u'.');
    auto it = suffixes.begin();
    const auto end = suffixes.end();
    while (it != end) {
	if (isValidSuffix(*it++))	// { dg-bogus "dangling pointer to .candidates. may be used" }
	  return true;
    }
    return false;
}
