/* { dg-do compile } */
/* { dg-options "-std=c++11 -w -O2 -fPIC" } */
namespace CLHEP {
      static const double meter = 1000.*10;
      static const double meter2 = meter*meter;
      static const double megaelectronvolt = 1. ;
      static const double gigaelectronvolt = 1.e+3;
      static const double GeV = gigaelectronvolt;
      static const double megavolt = megaelectronvolt;
      static const double volt = 1.e-6*megavolt;
      static const double tesla = volt*1.e+9/meter2;
    }
       using CLHEP::GeV;
       using CLHEP::tesla;
       namespace std {
      typedef long int ptrdiff_t;
    }
       extern "C" {
    extern double cos (double __x) throw ();
    extern double sin (double __x) throw ();
    extern double sqrt (double __x) throw ();
    }
       namespace std __attribute__ ((__visibility__ ("default"))) {
      using ::cos;
      using ::sin;
      using ::sqrt;
      template<class _CharT>     struct char_traits;
      template<typename _CharT, typename _Traits = char_traits<_CharT> >     struct basic_ostream;
      typedef basic_ostream<char> ostream;
      template<typename _Iterator>     struct iterator_traits     {      };
      template<typename _Tp>     struct iterator_traits<_Tp*>     {
        typedef ptrdiff_t difference_type;
        typedef _Tp& reference;
      };
    }
       namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
      using std::iterator_traits;
      template<typename _Iterator, typename _Container>     struct __normal_iterator     {
    _Iterator _M_current;
        typedef iterator_traits<_Iterator> __traits_type;
        typedef typename __traits_type::difference_type difference_type;
        typedef typename __traits_type::reference reference;
        explicit       __normal_iterator(const _Iterator& __i)       : _M_current(__i) {  }
        reference       operator*() const       {
  return *_M_current;
  }
        __normal_iterator       operator+(difference_type __n) const       {
  return __normal_iterator(_M_current + __n);
  }
      };
      template<typename _Tp>     struct new_allocator     {
      };
    }
       namespace std __attribute__ ((__visibility__ ("default"))) {
      template<typename _Tp>     struct allocator: public __gnu_cxx::new_allocator<_Tp>     {
    };
      struct ios_base   {      };
      template<typename _CharT, typename _Traits>     struct basic_ios : public ios_base     {      };
      template<typename _CharT, typename _Traits>     struct basic_ostream : virtual public basic_ios<_CharT, _Traits>     {
        typedef basic_ostream<_CharT, _Traits> __ostream_type;
        __ostream_type&       operator<<(__ostream_type& (*__pf)(__ostream_type&))       {  }
        __ostream_type&       operator<<(const void* __p)       {
  return _M_insert(__p);
  }
        template<typename _ValueT>  __ostream_type&  _M_insert(_ValueT __v);
      };
      template<typename _CharT, typename _Traits>     inline basic_ostream<_CharT, _Traits>&     endl(basic_ostream<_CharT, _Traits>& __os)     {
  }
    }
       typedef double G4double;
       typedef int G4int;
         extern __thread std::ostream *G4cout_p;
       struct G4Field;
       struct G4FieldManager {
         inline  G4Field* GetDetectorField() ;
    };
       namespace CLHEP {
    struct Hep3Vector {
    Hep3Vector(double x, double y, double z);
    inline ~Hep3Vector();
    inline double x() const;
    inline double y() const;
    inline double z() const;
    inline double mag() const;
    inline Hep3Vector cross(const Hep3Vector &) const;
  double dx;
    double dy;
    double dz;
  };
    Hep3Vector operator / (const Hep3Vector &, double a);
    inline double Hep3Vector::x() const {
  return dx;
  }
    inline double Hep3Vector::y() const {
  return dy;
  }
    inline double Hep3Vector::z() const {
  return dz;
  }
    inline Hep3Vector operator + (const Hep3Vector & a, const Hep3Vector & b) {  }
    inline Hep3Vector operator * (const Hep3Vector & p, double a) {  }
    inline double operator * (const Hep3Vector & a, const Hep3Vector & b) {  }
    inline Hep3Vector::Hep3Vector(double x1, double y1, double z1)   : dx(x1), dy(y1), dz(z1) {
 }
    inline Hep3Vector::~Hep3Vector() {  }
    inline Hep3Vector Hep3Vector::cross(const Hep3Vector & p) const {
    return Hep3Vector(dy*p.dz-p.dy*dz, dz*p.dx-p.dz*dx, dx*p.dy-p.dx*dy);
  }
    }
       typedef CLHEP::Hep3Vector G4ThreeVector;
       namespace std __attribute__ ((__visibility__ ("default"))) {
      template<typename _Tp, typename _Alloc = std::allocator<_Tp> >     struct vector
  {
        typedef _Tp *pointer;
        typedef __gnu_cxx::__normal_iterator<pointer, vector> iterator;
        iterator       begin()       {   }
      };
    }
       struct G4TransportationManager {
 static G4TransportationManager* GetTransportationManager();
         inline G4FieldManager* GetFieldManager() const;
    };
       struct G4ErrorMatrix {
       G4ErrorMatrix(G4int p, G4int q, G4int i);
       virtual ~G4ErrorMatrix();
       struct G4ErrorMatrix_row    {
    inline G4ErrorMatrix_row(G4ErrorMatrix&,G4int);
         G4double & operator[](G4int);
     G4ErrorMatrix& _a;
         G4int _r;
     };
       inline G4ErrorMatrix_row operator[] (G4int);
       std::vector<G4double > m;
       G4int nrow, ncol;
    };
       inline G4ErrorMatrix::G4ErrorMatrix_row G4ErrorMatrix::operator[] (G4int r) {
      G4ErrorMatrix_row b(*this,r);
      return b;
    }
       inline G4double &G4ErrorMatrix::G4ErrorMatrix_row::operator[](G4int c) {
      return *(_a.m.begin()+_r*_a.ncol+c);
    }
       inline G4ErrorMatrix:: G4ErrorMatrix_row::G4ErrorMatrix_row(G4ErrorMatrix&a, G4int r)    : _a(a) {
      _r = r;
    };
       struct G4DynamicParticle {
         G4double GetCharge() const;
    };
       struct G4Step;
       struct G4Track {
       const G4DynamicParticle* GetDynamicParticle() const;
       const G4ThreeVector& GetPosition() const;
       G4ThreeVector GetMomentum() const;
       const G4Step* GetStep() const;
    };
       struct G4StepPoint {
  const G4ThreeVector& GetPosition() const;
       G4ThreeVector GetMomentum() const;
    };
       struct G4Step {
       G4StepPoint* GetPreStepPoint() const;
       G4double GetStepLength() const;
    };
       namespace HepGeom {
      template<class T> struct BasicVector3D {
     T v_[3];
      BasicVector3D(T x1, T y1, T z1) {      }
      operator T * () {
  return v_;
  }
      T x() const {
  return v_[0];
  }
      T y() const {
  return v_[1];
  }
      T z() const {
  return v_[2];
  }
      T perp2() const {  }
      T perp() const {
  return std::sqrt(perp2());
      }
      T mag2() const {  }
      T mag() const {
  return std::sqrt(mag2());
  }
      T theta() const {      }
    };
      inline BasicVector3D<double>   operator-(const BasicVector3D<double> & a,const BasicVector3D<double> & b) {    }
      inline BasicVector3D<double>   operator*(const BasicVector3D<double> & v, double a) {    }
      template<class T>   struct Point3D : public BasicVector3D<T> {
      explicit Point3D(const double * a)       : BasicVector3D<double>(a[0],a[1],a[2]) { }
      Point3D(const CLHEP::Hep3Vector & v)       : BasicVector3D<double>(v.dx,v.dy,v.dz) {      }
    };
    }
       typedef HepGeom::Point3D<G4double> G4Point3D;
       namespace HepGeom {
      template<class T>   struct Vector3D : public BasicVector3D<T> {
      Vector3D(const BasicVector3D<double> & v) : BasicVector3D<double>(v) { }
      Vector3D(const CLHEP::Hep3Vector & v)       : BasicVector3D<double>(v.dx,v.dy,v.dz) { }
      operator CLHEP::Hep3Vector () const {      }
    };
    }
       typedef HepGeom::Vector3D<G4double> G4Vector3D;
       struct G4ErrorFreeTrajState 
{
      virtual G4int PropagateError( const G4Track* aTrack );
      G4int PropagateErrorMSC( const G4Track* aTrack );
    };
       G4int G4ErrorFreeTrajState::PropagateError( const G4Track* aTrack ) {
      G4double stepLengthCm = aTrack->GetStep()->GetStepLength()/10.;
      G4Point3D vposPost = aTrack->GetPosition()/10.;
      G4Vector3D vpPost = aTrack->GetMomentum()/GeV;
      G4Point3D vposPre = aTrack->GetStep()->GetPreStepPoint()->GetPosition()/10.;
      G4Vector3D vpPre = aTrack->GetStep()->GetPreStepPoint()->GetMomentum()/GeV;
      G4double pPre = vpPre.mag();
      G4double pPost = vpPost.mag();
      G4double pInvPre = 1./pPre;
      G4double pInvPost = 1./pPost;
      G4double deltaPInv = pInvPost - pInvPre;
      G4Vector3D vpPreNorm = vpPre * pInvPre;
      G4Vector3D vpPostNorm = vpPost * pInvPost;
 (*G4cout_p) << "G4EP: vpPreNorm " << vpPreNorm << " vpPostNorm " << vpPostNorm << std::endl;
      G4double sinpPre = std::sin( vpPreNorm.theta() );
      G4double sinpPostInv = 1./std::sin( vpPreNorm.theta() );
      G4ErrorMatrix transf(5, 5, 0 );
      G4double charge = aTrack->GetDynamicParticle()->GetCharge();
      G4double h1[3], h2[3];
 G4Field* field 
= G4TransportationManager::GetTransportationManager()->GetFieldManager()->GetDetectorField()
;
      if( charge != 0. && field )
 {
      G4ThreeVector HPre = G4ThreeVector( h1[0], h1[1], h1[2] ) / tesla *10.;
      G4ThreeVector HPost= G4ThreeVector( h2[0], h2[1], h2[2] ) / tesla *10.;
 {
      G4double pInvAver = 1./(pInvPre + pInvPost );
      G4double CFACT8 = 2.997925E-4;
      G4ThreeVector vHAverNorm( (HPre*pInvPre + HPost*pInvPost ) * pInvAver * charge * CFACT8 );
      G4double HAver = vHAverNorm.mag();
      G4double pAver = (pPre+pPost)*0.5;
      G4double QAver = -HAver/pAver;
      G4double thetaAver = QAver * stepLengthCm;
      G4double sinThetaAver = std::sin(thetaAver);
      G4double cosThetaAver = std::cos(thetaAver);
      G4double gamma = vHAverNorm * vpPostNorm;
      G4ThreeVector AN2 = vHAverNorm.cross( vpPostNorm );
      G4double AU = 1./vpPreNorm.perp();
      G4ThreeVector vUPre( -AU*vpPreNorm.y(),                       AU*vpPreNorm.x(),                       0. );
      G4ThreeVector vVPre( -vpPreNorm.z()*vUPre.y(),                       vpPreNorm.z()*vUPre.x(),                       vpPreNorm.x()*vUPre.y() - vpPreNorm.y()*vUPre.x() );
      AU = 1./vpPostNorm.perp();
      G4ThreeVector vUPost( -AU*vpPostNorm.y(),                        AU*vpPostNorm.x(),                        0. );
      G4ThreeVector vVPost( -vpPostNorm.z()*vUPost.y(),                        vpPostNorm.z()*vUPost.x(),                        vpPostNorm.x()*vUPost.y() - vpPostNorm.y()*vUPost.x() );
      G4Point3D deltaPos( vposPre - vposPost );
      G4double QP = QAver * pAver;
      G4double ANV = -( vHAverNorm.x()*vUPost.x() + vHAverNorm.y()*vUPost.y() );
      G4double ANU = ( vHAverNorm.x()*vVPost.x() + vHAverNorm.y()*vVPost.y() + vHAverNorm.z()*vVPost.z() );
      G4double OMcosThetaAver = 1. - cosThetaAver;
      G4double TMSINT = thetaAver - sinThetaAver;
      G4ThreeVector vHUPre( -vHAverNorm.z() * vUPre.y(),                           vHAverNorm.z() * vUPre.x(),                           vHAverNorm.x() * vUPre.y() - vHAverNorm.y() * vUPre.x() );
      G4ThreeVector vHVPre( vHAverNorm.y() * vVPre.z() - vHAverNorm.z() * vVPre.y(),                           vHAverNorm.z() * vVPre.x() - vHAverNorm.x() * vVPre.z(),                           vHAverNorm.x() * vVPre.y() - vHAverNorm.y() * vVPre.x() );
      transf[0][1] = -deltaPInv/thetaAver*       ( TMSINT*gamma*(vHAverNorm.x()*vVPre.x()+vHAverNorm.y()*vVPre.y()+vHAverNorm.z()*vVPre.z()) +         sinThetaAver*(vVPre.x()*vpPostNorm.x()+vVPre.y()*vpPostNorm.y()+vVPre.z()*vpPostNorm.z()) +         OMcosThetaAver*(vHVPre.x()*vpPostNorm.x()+vHVPre.y()*vpPostNorm.y()+vHVPre.z()*vpPostNorm.z()) );
      transf[0][2] = -sinpPre*deltaPInv/thetaAver*       ( TMSINT*gamma*(vHAverNorm.x()*vUPre.x()+vHAverNorm.y()*vUPre.y() ) +         sinThetaAver*(vUPre.x()*vpPostNorm.x()+vUPre.y()*vpPostNorm.y() ) +         OMcosThetaAver*(vHUPre.x()*vpPostNorm.x()+vHUPre.y()*vpPostNorm.y()+vHUPre.z()*vpPostNorm.z()) );
      transf[0][3] = -deltaPInv/stepLengthCm*(vUPre.x()*vpPostNorm.x()+vUPre.y()*vpPostNorm.y() );
      transf[1][1] = cosThetaAver*(vVPre.x()*vVPost.x()+vVPre.y()*vVPost.y()+vVPre.z()*vVPost.z()) +       sinThetaAver*(vHVPre.x()*vVPost.x()+vHVPre.y()*vVPost.y()+vHVPre.z()*vVPost.z()) +       OMcosThetaAver*(vHAverNorm.x()*vVPre.x()+vHAverNorm.y()*vVPre.y()+vHAverNorm.z()*vVPre.z())*       (vHAverNorm.x()*vVPost.x()+vHAverNorm.y()*vVPost.y()+vHAverNorm.z()*vVPost.z()) +       ANV*( -sinThetaAver*(vVPre.x()*vpPostNorm.x()+vVPre.y()*vpPostNorm.y()+vVPre.z()*vpPostNorm.z()) +             OMcosThetaAver*(vVPre.x()*AN2.x()+vVPre.y()*AN2.y()+vVPre.z()*AN2.z()) -             TMSINT*gamma*(vHAverNorm.x()*vVPre.x()+vHAverNorm.y()*vVPre.y()+vHAverNorm.z()*vVPre.z()) );
      transf[1][2] = cosThetaAver*(vUPre.x()*vVPost.x()+vUPre.y()*vVPost.y() ) +       sinThetaAver*(vHUPre.x()*vVPost.x()+vHUPre.y()*vVPost.y()+vHUPre.z()*vVPost.z()) +       OMcosThetaAver*(vHAverNorm.x()*vUPre.x()+vHAverNorm.y()*vUPre.y() )*       (vHAverNorm.x()*vVPost.x()+vHAverNorm.y()*vVPost.y()+vHAverNorm.z()*vVPost.z()) +       ANV*( -sinThetaAver*(vUPre.x()*vpPostNorm.x()+vUPre.y()*vpPostNorm.y() ) +             OMcosThetaAver*(vUPre.x()*AN2.x()+vUPre.y()*AN2.y() ) -             TMSINT*gamma*(vHAverNorm.x()*vUPre.x()+vHAverNorm.y()*vUPre.y() ) );
      transf[2][0] = -QP*ANU*(vpPostNorm.x()*deltaPos.x()+vpPostNorm.y()*deltaPos.y()+vpPostNorm.z()*deltaPos.z())*sinpPostInv       *(1.+deltaPInv*pAver);
      transf[2][3] = -QAver*ANU*(vUPre.x()*vpPostNorm.x()+vUPre.y()*vpPostNorm.y() )*sinpPostInv;
      transf[3][4] = (vVPre.x()*vUPost.x()+vVPre.y()*vUPost.y() );
      transf[4][0] = pAver*(vVPost.x()*deltaPos.x()+vVPost.y()*deltaPos.y()+vVPost.z()*deltaPos.z())       *(1.+deltaPInv*pAver);
      transf[4][1] = ( sinThetaAver*(vVPre.x()*vVPost.x()+vVPre.y()*vVPost.y()+vVPre.z()*vVPost.z()) +                        OMcosThetaAver*(vHVPre.x()*vVPost.x()+vHVPre.y()*vVPost.y()+vHVPre.z()*vVPost.z()) +                        TMSINT*(vHAverNorm.x()*vVPost.x()+vHAverNorm.y()*vVPost.y()+vHAverNorm.z()*vVPost.z())*                        (vHAverNorm.x()*vVPre.x()+vHAverNorm.y()*vVPre.y()+vHAverNorm.z()*vVPre.z()) )/QAver;
      transf[4][2] = ( sinThetaAver*(vUPre.x()*vVPost.x()+vUPre.y()*vVPost.y() ) +                        OMcosThetaAver*(vHUPre.x()*vVPost.x()+vHUPre.y()*vVPost.y()+vHUPre.z()*vVPost.z()) +                        TMSINT*(vHAverNorm.x()*vVPost.x()+vHAverNorm.y()*vVPost.y()+vHAverNorm.z()*vVPost.z())*                        (vHAverNorm.x()*vUPre.x()+vHAverNorm.y()*vUPre.y() ) )*sinpPre/QAver;
     }
    }
       PropagateErrorMSC( aTrack );
    }
