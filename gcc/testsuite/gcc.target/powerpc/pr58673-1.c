/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

enum typecode
{
  QIcode, QUcode, HIcode, HUcode, SIcode, SUcode, DIcode, DUcode, SFcode,
    DFcode, XFcode, Pcode, Tcode, LAST_AND_UNUSED_TYPECODE
};
enum bytecode_opcode
{
  neverneverland, drop, duplicate, over, setstackSI, adjstackSI, constQI,
    constHI, constSI, constDI, constSF, constDF, constXF, constP, loadQI,
    loadHI, loadSI, loadDI, loadSF, loadDF, loadXF, loadP, storeQI, storeHI,
    storeSI, storeDI, storeSF, storeDF, storeXF, storeP, storeBLK, clearBLK,
    addconstPSI, newlocalSI, localP, argP, convertQIHI, convertHISI,
    convertSIDI, convertQISI, convertQUHU, convertHUSU, convertSUDU,
    convertQUSU, convertSFDF, convertDFXF, convertHIQI, convertSIHI,
    convertDISI, convertSIQI, convertSUQU, convertDFSF, convertXFDF,
    convertSISF, convertSIDF, convertSIXF, convertSUSF, convertSUDF,
    convertSUXF, convertDISF, convertDIDF, convertDIXF, convertDUSF,
    convertDUDF, convertDUXF, convertSFSI, convertDFSI, convertXFSI,
    convertSFSU, convertDFSU, convertXFSU, convertSFDI, convertDFDI,
    convertXFDI, convertSFDU, convertDFDU, convertXFDU, convertPSI,
    convertSIP, convertSIT, convertDIT, convertSFT, convertDFT, convertXFT,
    convertPT, zxloadBI, sxloadBI, sstoreBI, addSI, addDI, addSF, addDF,
    addXF, addPSI, subSI, subDI, subSF, subDF, subXF, subPP, mulSI, mulDI,
    mulSU, mulDU, mulSF, mulDF, mulXF, divSI, divDI, divSU, divDU, divSF,
    divDF, divXF, modSI, modDI, modSU, modDU, andSI, andDI, iorSI, iorDI,
    xorSI, xorDI, lshiftSI, lshiftSU, lshiftDI, lshiftDU, rshiftSI, rshiftSU,
    rshiftDI, rshiftDU, ltSI, ltSU, ltDI, ltDU, ltSF, ltDF, ltXF, ltP, leSI,
    leSU, leDI, leDU, leSF, leDF, leXF, leP, geSI, geSU, geDI, geDU, geSF,
    geDF, geXF, geP, gtSI, gtSU, gtDI, gtDU, gtSF, gtDF, gtXF, gtP, eqSI,
    eqDI, eqSF, eqDF, eqXF, eqP, neSI, neDI, neSF, neDF, neXF, neP, negSI,
    negDI, negSF, negDF, negXF, notSI, notDI, notT, predecQI, predecHI,
    predecSI, predecDI, predecP, predecSF, predecDF, predecXF, predecBI,
    preincQI, preincHI, preincSI, preincDI, preincP, preincSF, preincDF,
    preincXF, preincBI, postdecQI, postdecHI, postdecSI, postdecDI, postdecP,
    postdecSF, postdecDF, postdecXF, postdecBI, postincQI, postincHI,
    postincSI, postincDI, postincP, postincSF, postincDF, postincXF,
    postincBI, xjumpif, xjumpifnot, jump, jumpP, caseSI, caseSU, caseDI,
    caseDU, call, returnP, ret, linenote, LAST_AND_UNUSED_OPCODE
};
struct binary_operator
{
  enum bytecode_opcode opcode;
  enum typecode arg0;
};
static struct conversion_recipe
{
  unsigned char *opcodes;
  int cost;
}
conversion_recipe[((int) LAST_AND_UNUSED_TYPECODE)][((int)
						     LAST_AND_UNUSED_TYPECODE)];
static struct conversion_recipe
deduce_conversion (from, to)
     enum typecode from, to;
{
  (conversion_recipe[(int) from][(int) to].
   opcodes ? 0 : (conversion_recipe[(int) from][(int) to] =
		  deduce_conversion (from, to), 0));
}

void
bc_expand_binary_operation (optab, resulttype, arg0, arg1)
     int resulttype, arg0, arg1;
     struct binary_operator optab[];
{
  int i, besti, cost, bestcost;
  enum typecode resultcode, arg0code;
  for (i = 0; optab[i].opcode != -1; ++i)
    {
      (conversion_recipe[(int) arg0code][(int) optab[i].arg0].
       opcodes ? 0 : (conversion_recipe[(int) arg0code][(int) optab[i].arg0] =
		      deduce_conversion (arg0code, optab[i].arg0), 0));
    }
}
