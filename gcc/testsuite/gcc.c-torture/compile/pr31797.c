struct GTeth_desc
{
  unsigned ed_cmdsts;
};
struct GTeth_softc
{
  struct GTeth_desc txq_desc[32];
  unsigned int txq_fi;
  unsigned int txq_nactive;
};

void
GTeth_txq_free (struct GTeth_softc *sc)
{
  struct GTeth_desc *txd = &sc->txq_desc[0];
  txd->ed_cmdsts &= ~(1U << (31));
}
void
GTeth_txq_done (struct GTeth_softc *sc)
{
  while (sc->txq_nactive > 0)
    {
      volatile struct GTeth_desc *txd = &sc->txq_desc[sc->txq_fi];
      if (txd->ed_cmdsts)
	{
	  if (sc->txq_nactive == 1)
	    return;
	}
      GTeth_txq_free (sc);
    }
}
