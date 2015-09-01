// PR c++/61753

class Rulec {
  Rulec();
};

const Rulec::Rulec() { }      // { dg-error "1:qualifiers" }

class Rulev {
  Rulev();
};

volatile Rulev::Rulev() { }   // { dg-error "1:qualifiers" }

class Ruler {
  Ruler();
};

__restrict Ruler::Ruler() { }  // { dg-error "1:qualifiers" }

class Rulecvr {
  Rulecvr();
};

const volatile __restrict Rulecvr::Rulecvr() { }  // { dg-error "1:qualifiers" }

class Rulervc {
  Rulervc();
};

__restrict volatile const Rulervc::Rulervc() { }  // { dg-error "1:qualifiers" }
