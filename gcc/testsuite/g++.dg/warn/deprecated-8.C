// PR c++/58305

class ToBeDeprecated {
} __attribute__ ((deprecated ("deprecated!")));

typedef ToBeDeprecated NotToBeDeprecated; // { dg-warning "'ToBeDeprecated' is deprecated" }

int main() {

  ToBeDeprecated();    // { dg-warning "'ToBeDeprecated' is deprecated" }
  ToBeDeprecated x;    // { dg-warning "'ToBeDeprecated' is deprecated" }

  NotToBeDeprecated();
  NotToBeDeprecated y;
}
